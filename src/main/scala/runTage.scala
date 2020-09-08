package scalaTage

import scala.collection.mutable
// import scala.collection.immutable._
import scala.math._
import scala.util._
import sys.process._

object utils {
    def getLogs(path: String): Array[String] = {
        val files = s"ls $path".!!.split('\n')
        val logPattern = raw"log".r.unanchored
        files.map(f => if ((logPattern findAllIn f).length != 0) path+f else "").filter(_ != "").toArray
    }
}


class BranchPredictorRunner() {
    val tw = new TraceWrapper
    val bp = new Tage

    val maxBrPrint = 10

    def getCfis(file: String): Iterator[CFIInfo] = tw.getCFIInfosFromFile(file)

    def runWithCFIInfo(cfis: Iterator[CFIInfo]) = {
        // val cfiQueue = mutable.Queue(cfis: _*)
        println(f"Running with ${bp}%s")
        val mispred = new mutable.HashMap [Long, Int]
        val correct = new mutable.HashMap [Long, Int]
        cfis.foreach(i => 
            // we only care about branches
            if (i.isBr) {
                val pred = bp.predict(i.pc)
                bp.update(i.pc, i.taken, pred)
                if (i.taken != pred) {
                    if (mispred.contains(i.pc)) mispred(i.pc) += 1
                    else mispred += (i.pc -> 1)
                }
                else {
                    if (correct.contains(i.pc)) correct(i.pc) += 1
                    else correct += (i.pc -> 1)
                }
            }
        )

        def getAndPrintPreds(stat: mutable.HashMap [Long, Int], isCorr: Boolean): Int = {
            var brPrint = 0
            var totalPred = 0
            stat.toList.sortBy(_._2).reverse.foreach{ case(pc, n) => {
                if (brPrint < maxBrPrint) { println(f"pc: $pc%x ${if(isCorr) "correctly " else "mis"}%spredicted for $n%d times"); brPrint += 1; }
                totalPred += n
            }}
            totalPred
        }
        println(f"Printing top $maxBrPrint%d branches:")

        val totalMispred: Int = getAndPrintPreds(mispred, false)
        val totalCorrect: Int = getAndPrintPreds(correct, true)
        val totalBrs = totalMispred + totalCorrect

        println(f"Totally mispredicted $totalMispred%d out of $totalBrs%d branches")
        // tage.flush
        (totalCorrect, totalMispred)
    }

    def runWithLogs(logs: Array[String]): Array[(String, (Int, Int))] = {
        logs.map(l => {println(s"processing log $l"); (l, runWithCFIInfo(getCfis(l)));}).toArray
    }

    def printRes(res: Array[(String, (Int, Int))]) = {
        res.foreach { case(l, (c, m)) => {
            println(f"test: ${l.split('/').last}%20s, $m%6d/${c+m}%8d mispredicted, rate: ${m.toDouble/(c+m)}%6f")
        }}
    }

}

object TageRunnerTest {
    def main(args: Array[String]): Unit = {
        val bpr = new BranchPredictorRunner
        // val logs = utils.getLogs("/home/glr/nexus-am/tests/cputest/build/")
        // val logs = Array("/home/glr/XiangShan/debug/dhrystone.log")
        val logs = Array("/home/glr/XiangShan/debug/coremark.log")
        // val logs = Array("/home/glr/XiangShan/debug/coremark10.log")
        // val logs = Array("/home/glr/XiangShan/debug/microbench.log")
        // val logs = (0 until 10).map(_ => "/home/glr/XiangShan/debug/coremark.log").toArray
        logs.foreach(println)
        val res = bpr.runWithLogs(logs)
        bpr.printRes(res)

        // tr.runWithCFIInfo(tr.cfis("/home/glr/nexus-am/tests/cputest/build/bubble-sort.log"))
    }
}