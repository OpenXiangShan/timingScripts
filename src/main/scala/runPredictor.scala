package scalaTage

import scala.collection.mutable
import scala.util.matching.Regex
// import scala.collection.immutable._
import scala.math._
import scala.util._
import sys.process._
import java.io.File

trait ArgParser {
    val usage = """
        Usage: [--log-in-debug logname] | [--run-cputest]
    """
    type OptionMap = Map[Symbol, Any]
    
    def getLogs(path: String): Array[String] = {
        val files = s"ls $path".!!.split('\n')
        val logPattern = raw"log".r.unanchored
        files.map(f => if ((logPattern findAllIn f).length != 0) path+f else "").filter(_ != "").toArray
    }

    def parse(args: Array[String]) = {
        if (args.length == 0) println(usage)
        val arglist = args.toList

        @scala.annotation.tailrec
        def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
            def isSwitch(s : String)= (s(0) == '-')
            def fileToPathInDebug(file: String) = "/home/glr/XiangShan/debug/" + file + ".log"
            list match {
                case Nil => map
                case "--log-in-debug" :: file :: tail => 
                    nextOption(map ++ Map('file -> fileToPathInDebug(file)), tail)
                case "--run-cputest" :: tail =>
                    nextOption(map ++ Map('multipleFiles -> getLogs("/home/glr/nexus-am/tests/cputest/build/")), tail)
                case "--his" :: value :: tail =>
                    nextOption(map ++ Map('hislen -> value.toInt), tail)
                case "--updateOnUncond" :: tail =>
                    nextOption(map ++ Map('updateOnUncond -> true), tail)
                case "--withLoop" :: tail =>
                    nextOption(map ++ Map('withLoop -> true), tail)
                case "--superscalar" :: tail =>
                    nextOption(map ++ Map('superscalar -> true), tail)
                case "--useGem5" :: tail =>
                    nextOption(map ++ Map('useGem5 -> true), tail)
                case "--useXS" :: tail =>
                    nextOption(map ++ Map('useXS -> true), tail)
                // case string :: opt2 :: tail if isSwitch(opt2) => 
                //                     nextOption(map ++ Map('infile -> string), list.tail)
                // case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
                case option :: tail => { println("Unknown option "+option); nextOption(map, list.tail); }
            }
        }
        nextOption(Map(),arglist)
    }
}

trait RunnerUtils {
    this: ArgParser =>
    def getName(ops: OptionMap): String = ""
}


class BranchPredictorRunner extends RunnerUtils with ArgParser {
    val tw = new TraceWrapper

    val maxBrPrint = 10

    val defaultInput = "/home/glr/XiangShan/debug/dhrystone.log"

    def getCfis(file: String): Iterator[Any] = tw.getCFIInfosFromFile(file)

    def runWithCFIInfo(cfis: Iterator[Any])(implicit bp: BasePredictor) = {     
        //                               pc  , (mis, cor)
        val stats = new mutable.HashMap [Long, Array[Int]] 
        cfis.foreach { cfi => cfi match {
            case CFIInfo(isBr, pc, taken, misPred) => {
                    // we only care about branches
                    val pred = bp.predict(pc, isBr)
                    if (isBr) {
                        bp.update(pc, taken, pred)
                        if (taken != pred) {
                            if (stats.contains(pc)) stats(pc)(0) += 1
                            else stats += (pc -> Array(1, 0))
                        }
                        else {
                            if (stats.contains(pc)) stats(pc)(1) += 1
                            else stats += (pc -> Array(0, 1))
                        }
                    } else {
                        bp.updateUncond(pc)
                    }
                }
            case _ => 
            }
        }

        def getAndPrintPreds(stats: mutable.HashMap [Long, Array[Int]]): (Int, Int, Int) = {
            var brPrint = 0
            var totalPred = 0
            var totalMiss = 0
            var totalCorr = 0
            stats.toList.sortBy(_._2(0)).reverse.foreach{ case(pc, arr) => {
                val miss = arr(0)
                val corr = arr(1)
                if (brPrint < maxBrPrint) { println(f"pc: $pc%x, mispredict: ${miss}%10d, correct: ${corr}%10d, total: ${miss+corr}%10d, missrate: ${miss*100/(miss+corr).toDouble}%3.2f%%"); brPrint += 1; }
                totalMiss += arr(0)
                totalCorr += arr(1)
                totalPred += arr.reduce(_+_)
            }}
            (totalMiss, totalCorr, totalPred)
        }
        println(f"Printing top $maxBrPrint%d mispredicted branches:")

        val (totalMispred, totalCorrect, totalBrs) = getAndPrintPreds(stats)

        println(f"Totally mispredicted $totalMispred%d out of $totalBrs%d branches")

        // after one program is done, flush predictor for the next program
        bp.flush

        (totalCorrect, totalMispred)
    }

    def runWithLog(log: String)(implicit bp: BasePredictor): (String, (Int, Int)) = {
        val l = new File(log)
        if (l.exists()) {
            println(s"processing log $l")
            (log, runWithCFIInfo(getCfis(log)))
        }
        else {
            println(s"$l not exists, returning null result")
            (log, (1, 1))
        }
    }

    def runWithLogs(logs: Array[String])(implicit bp: BasePredictor): Array[(String, (Int, Int))] = logs.map(runWithLog(_)).toArray

    def printRes(res: Array[(String, (Int, Int))]) = {
        res.foreach { case(l, (c, m)) => {
            println(f"test: ${l.split('/').last}%20s, $m%6d/${c+m}%8d mispredicted, missrate: ${(m*100).toDouble/(c+m)}%3.3f%%")
        }}
    }

    def checkOps(ops: OptionMap) = {
        if (ops.contains('file) && ops.contains('multipleFiles)) {
            println("Conflict arguments, you could only use --log-in-debug OR --run-cputest")
            System.exit(1)
        }
    }

    def run(ops: OptionMap) = {
        implicit val bp = Tage(ops)
        println(f"Running with ${bp}%s")
        checkOps(ops)
        val res = 
            if (ops.contains('file)) {
                Array(runWithLog(ops('file).toString))
            }
            else if (ops.contains('multipleFiles)) {
                runWithLogs(ops('multipleFiles).asInstanceOf[Array[String]])
            }
            else {
                println("No input specified, running on default dhrystone\n")
                Array(runWithLog(defaultInput))
            }
        printRes(res)
    }
}





object BranchPredictorRunnerTest extends RunnerUtils with ArgParser {
    def main(args: Array[String]): Unit = {
        val options = parse(args)
        val bpr = new BranchPredictorRunner
        bpr.run(options)
    }
}