package scalaTage

import scala.collection.mutable
// import scala.collection.immutable._
import scala.math._
import scala.util._

case class PerceptronParams (
    val row: Int = 256,
    val hislen: Int = 50,
    val ctrbits: Int = 8
) {
    val col = hislen
    val threshold = (1.93 * hislen + 14).toInt
    def totalKB = row * (col + 1) * ctrbits / 8192
    override def toString: String = f"${row}%d rows, ${col}%d columns, each perceptron has ${ctrbits}%d bits, threshold is ${threshold}%d\n" +
        f"Totally consumed ${totalKB}%dKB"
}

class PerceptronBP()(implicit val p: PerceptronParams) extends BasePredictor {
    override val updateOnUncond = true

    val row       = p.row
    val col       = p.col
    val hislen    = p.hislen
    val ctrbits   = p.ctrbits
    val threshold = p.threshold
    
    val rowMask = getMask(log2Up(row))
    val colMask = getMask(log2Up(hislen))

    val mem = Array.fill[Array[SatCounter]](row)(Array.fill[SatCounter](col)(SatCounter(ctrbits,1 << (ctrbits-1))))
    val bias = Array.fill[SatCounter](row)(SatCounter(ctrbits, 1 << (ctrbits-1)))
    val ghist = new GlobalHistory(hislen)

    case class PerceptronMeta (
        val pc: Long,
        val hist: List[Boolean],
        val pred: Boolean,
        val predSum: Int
    ) extends PredictionMeta {}

    val obq = new mutable.Queue[PerceptronMeta]

    def getRow(pc: Long): Int = ((pc >>> 1) % row).toInt

    def dotProduct(hist: List[Boolean], ctrs: List[SatCounter]): Int = {
        (hist zip ctrs) map { case (h, v) => v(h) } reduce (_+_)
    }

    def predict(pc: Long, isBr: Boolean): Boolean = {
        if (isBr || updateOnUncond) {
            val r = getRow(pc)
            val hist = ghist.getHist().toList
            val predSum = dotProduct(hist, mem(r).toList) + bias(r)()
            val pred = predSum >= 0
            obq.enqueue(PerceptronMeta(pc, hist, pred, predSum))
            pred
        }
        else true
    }

    def update(pc: Long, taken: Boolean): Boolean = {
        val meta = obq.head
        val pred = meta.pred
        if (pc != meta.pc) {
            println("update pc does not correspond with expected pc\n")
        } else {
            obq.dequeue()
            val misPred = taken != pred
            val r = getRow(pc)
            if (misPred || abs(meta.predSum) <= threshold) {
                bias(r) = bias(r).update(taken)
                def updateArray(e : (Boolean, SatCounter)): SatCounter = e._2.update(e._1 == taken)
                mem(r) = ((meta.hist zip mem(r).toList) map {updateArray(_)}).toArray
            }
            ghist.updateHist(taken)
        }
        pred
    }

    def updateUncond(pc: Long): Unit = {
        if (updateOnUncond) {
            val meta = obq.dequeue()
            ghist.updateHist(true)
        }
    }

    def flush() = {
        for (r <- 0 until row) {
            bias(r) = SatCounter(ctrbits, 1 << (ctrbits-1))
            for (c <- 0 until col) {
                mem(r)(c) = SatCounter(ctrbits, 1 << (ctrbits-1))
            }
        }
    }

    def name: String = "PERCEPTRON_BP"
    override def toString: String = f"${this.name}%s with params: \n${p}%s\nUsing global history ${if(updateOnUncond) "with" else "without"}%s jumps\n"
}

object PerceptronBP {
    def apply(ops: Map[Symbol, Any]): PerceptronBP = {
        implicit val p = PerceptronParams()
        new PerceptronBP()
    }
}