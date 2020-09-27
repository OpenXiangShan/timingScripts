package scalaTage

import scala.io.Source
import scala.util.matching.Regex
import scala._
import scala.collection.mutable

case class CFIUpdateInfo( 
    val cycle: Long,
    val isBr: Boolean,
    val pc: Long,
    val taken: Boolean,
    val misPred: Boolean,
    val pcycle: Long,
    val hist: Long
){
    override def toString: String = f"cycle($cycle%d), isBr($isBr%b) pc($pc%x) taken($taken%b) mispred($misPred%b), pcycle($pcycle%d), hist($hist%d)\n"
}

case class CFIPredInfo(
    val cycle: Long,
    val fetchpc: Long,
    val mask: Int,
    val brmask: Int,
    val hist: Long,
    val histPtr: Int
){
    override def toString: String = f"cycle($cycle%d), fetchpc($fetchpc%x), mask(${mask.toBinaryString}%s)\n"
}

// This is a wrapper of trace produced by XiangShan BPU update,
// which wraps cfi_updates into class CFIUpdateInfo
class TraceWrapper() extends PredictorUtils {
    val cfiUpdatePattern = "cfi_update".r
    val cfiUpdateInfoExtractPattern = raw"\[time= *([0-9]+)\].*isBr\(([0-1])\) pc\(([0-9|a-f]{10})\) taken\(([0-1])\) mispred\(([0-1])\) cycle\( *([0-9]+)\) hist\( *([0-9|a-f]+)\)".r.unanchored
    val cfiPredInfoExtractPattern = raw"\[time= *([0-9]+)\].*cfi_pred: fetchpc\(([0-9|a-f]{10})\) mask\( *([0-9]+)\) brmask\( *([0-9]+)\) hist\(([0-9|a-f]+)\) histPtr\( *([0-9]+)\)".r.unanchored

    def dumbCFI = CFIUpdateInfo(0, false, 0, false, false, 0, 0)
    def dumbPred = CFIPredInfo(0,0,0,0,0,0)

    def toBoolean(s: String): Boolean = 
        s match {
            case "1" => true
            case "0" => false
            case _ => {println("toBoolean error"); false}
        }

    def reMatch(str: String, p: Regex) = p findAllMatchIn str

    def getLines(file: String) = Source.fromFile(file).getLines()

    def getCFIUpdateInfo(u: String): Any = {
        u match {
            case cfiUpdateInfoExtractPattern(cycle, isBr, pc, taken, misPred, pcycle, hist) =>
                CFIUpdateInfo(cycle.toLong, toBoolean(isBr), java.lang.Long.parseLong(pc.trim(), 16), toBoolean(taken), toBoolean(misPred), pcycle.toLong, (new java.math.BigInteger(hist.trim(), 16)).longValue)
            case cfiUpdatePattern() => { println(" not a valid cfi_update line" + u); dumbCFI }
            case _ => 0 // not related lines
        }
    }

    def getCFIPredInfo(u: String): Any = {
        u match {
            case cfiPredInfoExtractPattern(cycle, fetchpc, mask, brmask, hist, histPtr) => {
                // println(f"pred info $u%s")
                CFIPredInfo(cycle.toLong, java.lang.Long.parseLong(fetchpc.trim(), 16), mask.toInt, brmask.toInt, (new java.math.BigInteger(hist.trim(), 16)).longValue, histPtr.toInt)
            }
            case _ => 0 // println(f"Unexpected pred info $u%s")
        }
    }

    def getCFIInfosFromFile(file: String, getInfo: String => Any): Iterator[Any] = getLines(file).map(getInfo(_))

    def getCFIPredInfosFromFile(file: String)   = getCFIInfosFromFile(file, getCFIPredInfo)
    def getCFIUpdateInfosFromFile(file: String) = getCFIInfosFromFile(file, getCFIUpdateInfo)

    def getXSResult(file: String): (Int, Int, Int, Int) = {
        var numBr = 0
        var numBrMispred = 0
        var numCFI = 0
        var numJMispred = 0
        val cfis = getCFIUpdateInfosFromFile(file)
        cfis.foreach { 
            case CFIUpdateInfo(cycle, isBr, _, _, misp, pcycle, hist) => {
                numCFI += 1
                if (isBr) {
                    numBr += 1
                    if (misp)
                        numBrMispred += 1
                }
                else {
                    if (misp)
                        numJMispred += 1
                }
            }
            case _ =>
        }
        println(f"In ${file.split('/').last}%s, totally $numBrMispred%d/$numBr%d branches mispredicted, totally $numJMispred%d/${numCFI-numBr}%d jumps mispredicted")
        (numBrMispred, numBr, numJMispred, numCFI)
    }

    def checkHist(file: String) = {
        var h: Long = 0
        val cfis = getCFIUpdateInfosFromFile(file)
        var prevCycle: Long = 0
        var thisShifted = false
        var count = 0
        var error = false
        cfis.foreach {
            case CFIUpdateInfo(cycle, isBr, _, taken, misp, pcycle, hist) if (!error) => {
                if (pcycle != prevCycle) {
                    if (h != hist) {
                        println(f"cycle $prevCycle%6d updated, hist is ${boolArrayToString(toBoolArray(h, 64))}%s, this cycle $cycle%d")
                        println(f"                    , hist is ${boolArrayToString(toBoolArray(hist, 64))}%s")
                        error = true
                    }
                    thisShifted = false
                    if (isBr) {
                        h = (h << 1L) | (if (taken) 1L else 0L)
                        thisShifted = true
                    }
                    prevCycle = pcycle
                }
                else {
                    if (isBr) {
                        if (!thisShifted) {
                            h = (h << 1L) | (if (taken) 1L else 0L)
                            thisShifted = true
                        }
                        else {
                            if (taken) h |= 1L
                        }
                    }
                    prevCycle = pcycle
                }
                count += 1
            }
            case _ =>
        }
        println(f"checked $count%d branches, ${if (!error) "no errors detected" else "got an error"}%s")
    }

    def checkPredHist(file: String) = {
        // val cfi_update = getCFIUpdateInfosFromFile("/home/glr/XiangShan/debug/coremark_update.log")
        // val cfi_pred = getCFIPredInfosFromFile("/home/glr/XiangShan/debug/coremark_pred.log")
        val cfi_update = getCFIUpdateInfosFromFile(file)
        val cfi_pred = getCFIPredInfosFromFile(file)        
        // in case multiple brs are predicted
        var br_count = 0
        var incorrect = 0
        var incorrect_misp = 0
        var unShifted = 0
        val preds = mutable.HashMap[Long, (Long, Int)]()
        cfi_update.foreach {
            case CFIUpdateInfo(cycle, isBr, _, _, misp, pcycle, hist) => {
                // println(f"pcycle is $pcycle%d")
                if (isBr) { br_count += 1 }
                if (preds.contains(pcycle)) {
                    val correct = hist == preds(pcycle)._1
                    if (!correct) {
                        if (isBr) {
                            incorrect += 1
                            if (misp) { incorrect_misp += 1}
                        }
                        println(f"pcycle($pcycle%d)hist ${if (correct) "=" else "!="}%s predhist")
                        println(f"update hist is ${boolArrayToString(toBoolArray(hist, 64))}%s")
                        println(f"  pred hist is ${boolArrayToString(toBoolArray(preds(pcycle)._1, 64))}%s")
                    }
                } else {
                    cfi_pred.find {
                        case CFIPredInfo(c, _, _, _, _, _) => {
                            // if (c < pcycle) { println(f"pred cycle $c%d dropped") }
                            c == pcycle
                        }
                        case _ => false
                    } match {
                        case Some(pred@CFIPredInfo(c, _, _, _, predhist, histPtr)) => {
                            preds(c) = (predhist, histPtr)
                            val correct = hist == predhist
                            if (!correct) {
                                if (isBr) {
                                    incorrect += 1
                                    if (misp) { incorrect_misp += 1}
                                }
                                println(f"pcycle($pcycle%d)hist ${if (correct) "=" else "!="}%s predhist")
                                println(f"update hist is ${boolArrayToString(toBoolArray(hist, 64))}%s")
                                println(f"  pred hist is ${boolArrayToString(toBoolArray(predhist, 64))}%s")
                            }
                        }
                        case None => {
                            println(f"totally find $incorrect%d branch pred hist errors out of $br_count%d brs, $incorrect_misp%d of them are mispredicted")
                            System.exit(0)
                        }
                    }
                }
            }
            case _ =>
        }
        println(f"totally find $incorrect%d branch pred hist errors out of $br_count%d brs, $incorrect_misp%d of them are mispredicted")

    }

    def checkUpdateCycle(file: String) = {
        val cfi_update = getCFIUpdateInfosFromFile(file)
        var c: Long = 0
        cfi_update.foreach {
            case CFIUpdateInfo(cycle, isBr, _, _, misp, pcycle, hist) => {
                if (pcycle < c) {
                    println(f"disoreder detected at cycle $cycle")
                }
                c = pcycle
            }
            case _ =>
        }
    }

}

// object WrapperTest{
//     def main(args: Array[String]): Unit = {
//         val tw = new TraceWrapper
//         val file = "/home/glr/nexus-am/tests/cputest/build/dummy.log"
//         tw.getCFIInfosFromFile(file).foreach(println)
//     }
// }

