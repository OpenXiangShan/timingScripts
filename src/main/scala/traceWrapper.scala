package scalaTage

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer

case class CFIInfo( 
    // val cycle: Int,
    val isBr: Boolean,
    val pc: Long,
    val taken: Boolean,
    val misPred: Boolean
){
    override def toString: String = f"isBr($isBr%b) pc($pc%x) taken($taken%b) mispred($misPred%b)\n"
}

// This is a wrapper of trace produced by XiangShan BPU update,
// which wraps cfi_updates into class CFIInfo
class TraceWrapper() {
    val cfiUpdatePattern = "cfi_update".r
    val cfiInfoExtractPattern = raw"isBr\(([0-1])\) pc\(([0-9|a-f]{10})\) taken\(([0-1])\) mispred\(([0-1])\)".r.unanchored

    def dumbCFI = CFIInfo(false, 0, false, false)

    def toBoolean(s: String): Boolean = 
        s match {
            case "1" => true
            case "0" => false
            case _ => {println("toBoolean error"); false}
        }

    def reMatch(str: String, p: Regex) = p findAllMatchIn str

    def getLines(file: String) = Source.fromFile(file).getLines()

    def getCFIInfo(u: String): Any = {
        u match {
            case cfiInfoExtractPattern(isBr, pc, taken, misPred) =>
                CFIInfo(toBoolean(isBr), java.lang.Long.parseLong(pc.trim(), 16), toBoolean(taken), toBoolean(misPred))
            case cfiUpdatePattern() => { println(" not a valid cfi_update line" + u); dumbCFI }
            case _ => 0 // not related lines
        }
    }

    def getCFIInfosFromFile(file: String): Iterator[Any] = getLines(file).map(getCFIInfo(_))

    def getXSResult(file: String): (Int, Int, Int, Int) = {
        var numBr = 0
        var numBrMispred = 0
        var numCFI = 0
        var numJMispred = 0
        val cfis = getCFIInfosFromFile(file)
        cfis.foreach { 
            case CFIInfo(isBr, _, _, misp) => {
                numCFI += 1
                if (isBr) {
                    numBr += 1
                    if (misp)
                        numBrMispred += 1
                }
                else {
                    if (misp)
                        numJMispred += 0
                }
            }
            case _ =>
        }
        println(f"In $file%s, totally $numBrMispred%d/$numBr%d branches mispredicted, totally $numJMispred%d/${numCFI-numBr}%d jumps mispredicted")
        (numBrMispred, numBr, numJMispred, numCFI)
    }

}

// object WrapperTest{
//     def main(args: Array[String]): Unit = {
//         val tw = new TraceWrapper
//         val file = "/home/glr/nexus-am/tests/cputest/build/dummy.log"
//         tw.getCFIInfosFromFile(file).foreach(println)
//     }
// }

