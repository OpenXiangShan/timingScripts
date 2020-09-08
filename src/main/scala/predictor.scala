package scalaTage

import scala.collection.mutable
// import scala.collection.immutable._
import scala.math._
import scala.util._


trait PredictorUtils {
    def satUpdate(taken: Boolean, oldCtr: Int, ctrBits: Int): Int = {
        oldCtr match {
            case x if (x >= ((1 << ctrBits) - 1) && taken) => (1 << ctrBits) - 1
            case y if (y <= 0 && !taken) => 0
            case _ => if (taken) {oldCtr + 1} else {oldCtr - 1}
        }
    }
    def isPowerOf2(x: Int): Boolean = (x & (x - 1)) == 0
    def log2(x: Int): Int = (log(x) / log(2)).toInt
    def log2Up(x: Int): Int = if (isPowerOf2(x)) log2(x) else log2(x) + 1
    def getMask(len: Int) = (1 << len) - 1
    def getBit(x: Long, pos: Int): Int = (x & (1L << pos)).toInt
    // from 1 to 3: get bits (1,2,3)
    def getBits(x: Long, from: Int, to: Int): Int = (from until to+1).map(i => getBit(x, i)).reduce(_|_) >>> from
    def divUp(x: Int, y: Int): Int = (x / y) + (if (x % y != 0) 1 else 0)

    def boolArrayToLong(arr: Array[Boolean]): Long = {
        // println(f"boolArrayToLong: arr size = ${arr.size}%d")
        arr.zipWithIndex.map{ case (b, i) => (if (b) 1L << i else 0L) }.reduce(_|_)
    }
    def boolArrayToInt(arr: Array[Boolean]): Int = boolArrayToLong(arr).toInt
    def toBoolArray(x: Long, len: Int): Array[Boolean] = {
        (0 until len).map(i => ((x >>> i) & 1) == 1).toArray
    }
    def toBoolArray(x: Int, len: Int): Array[Boolean] = {
        (0 until len).map(i => ((x >>> i) & 1) == 1).toArray
    }
    def PriorityEncoder(arr: Array[Boolean]): Int = {
        var res = arr.size - 1
        arr.zipWithIndex.reverse.foreach{case(b,i) => if (b) res = i}
        // println(f"arr is ${boolArrayToString(arr)}%s, res is $res")
        res
    }
    def PriorityEncoder(x: Int, len: Int): Int = PriorityEncoder(toBoolArray(x, len))

    def boolArrayToString(arr: Array[Boolean]): String = arr.map(if(_) "1" else "0").reduce(_+_)
}

abstract class BasePredictor extends PredictorUtils {
    def predict(pc: Long) : Boolean
    def update(pc: Long, taken: Boolean, pred: Boolean): Unit
    def name: String
}

abstract class PredictorComponents extends PredictorUtils {}

class GlobalHistory(val maxHisLen: Int) extends PredictorUtils {
    val arrMaxLen = maxHisLen * 2
    val hist: Array[Boolean] = Array.fill[Boolean](arrMaxLen)(false)
    var ptr: Int = 0
    var count: Int = 0
    def getHistPtr = this.ptr
    def getHist(len: Int = maxHisLen, ptr: Int = this.ptr): Array[Boolean] = {
        if (ptr - len >= 0)
            hist.slice(ptr-len, ptr).reverse
        else
            (hist.slice(ptr-len+arrMaxLen, arrMaxLen) ++ hist.slice(0, ptr)).reverse
    }

    def getHistStr(len: Int = maxHisLen, ptr: Int = this.ptr): String = boolArrayToString(this.getHist(len, ptr))

    def updateHist(taken: Boolean) = {
        count += 1
        hist.update(ptr, taken)
        ptr = if (ptr + 1 >= arrMaxLen) 0 else ptr + 1
    }

    def recover(oldPtr: Int, taken: Boolean) = {
        ptr = oldPtr
        updateHist(taken)
    }
}

trait GTimer {
    var cycle: Long = 0
    def step(x: Long) = cycle += x
    def isCycle(x: Long): Boolean = cycle == x
}