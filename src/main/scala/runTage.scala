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


class TageRunner() {
    val tw = new TraceWrapper
    val tage = new Tage

    def cfis(file: String) = tw.getCFIInfosFromFile(file)

    def runWithCFIInfo(cfis: Array[CFIInfo]) = {
        // val cfiQueue = mutable.Queue(cfis: _*)
        val mispred = new mutable.HashMap [Long, Int]
        val correct = new mutable.HashMap [Long, Int]
        cfis.foreach(i => 
            if (i.isBr) {
                val pred = tage.predict(i.pc)
                tage.update(i.pc, i.taken, pred)
                if (i.taken != pred) {
                    if (mispred.contains(i.pc)) {
                        mispred(i.pc) += 1
                    }
                    else {
                        mispred += (i.pc -> 1)
                    }
                }
                else {
                    if (correct.contains(i.pc)) {
                        correct(i.pc) += 1
                    }
                    else {
                        correct += (i.pc -> 1)
                    }
                }
            }
        )
        var totalMispred: Int = 0
        var totalCorrect: Int = 0
        var totalBrs: Int = 0
        mispred.foreach{ case(pc, n) => {
            // println(f"pc: $pc%x mispredicted for $n%d times")
            totalMispred += n
        }}
        correct.foreach{ case(pc, n) => {
            // println(f"pc: $pc%x correctly predicted for $n%d times")
            totalCorrect += n
        }}
        totalBrs = totalMispred + totalCorrect
        println(f"Totally mispredicted $totalMispred%d out of $totalBrs%d branches")
        tage.flush
        (totalCorrect, totalMispred)
    }

    def runWithLogs(logs: Array[String]): Array[(String, (Int, Int))] = {
        logs.map(l => {println(s"processing log $l"); (l, runWithCFIInfo(cfis(l)));}).toArray
    }

    def printRes(res: Array[(String, (Int, Int))]) = {
        res.foreach { case(l, (c, m)) => {
            println(f"test: ${l.split('/').last}%20s, $m%6d/${c+m}%8d mispredicted, rate: ${m.toDouble/(c+m)}%6f")
        }}
    }
    
}

object TageRunnerTest {
    def main(args: Array[String]): Unit = {
        val tr = new TageRunner
        val logs = utils.getLogs("/home/glr/nexus-am/tests/cputest/build/")
        logs.foreach(println)
        val res = tr.runWithLogs(logs)
        tr.printRes(res)

        // tr.runWithCFIInfo(tr.cfis("/home/glr/nexus-am/tests/cputest/build/bubble-sort.log"))
    }
}