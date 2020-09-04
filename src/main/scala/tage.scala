package scalaTage

import scala.collection.mutable
// import scala.collection.immutable._
import scala.math._
import scala.util._


trait TageParams {
    val BimEntries = 2048
    //                   Sets  Hist   Tag
    val TableInfo = Seq(( 128,    2,    7),
                        ( 128,    4,    7),
                        ( 256,    8,    8),
                        ( 256,   16,    8),
                        ( 128,   32,    9),
                        ( 128,   64,    9))
    val TageNTables = TableInfo.size
    val UBitPeriod = 8192
    val TageBanks = 16 // FetchWidth

    val TotalBits = TableInfo.map {
        case (s, h, t) => {
        s * (1+t+3+2) * 16
        }
    }.reduce(_+_)
}

trait Utils {
    def satUpdate(taken: Boolean, oldCtr: Int, ctrBits: Int): Int = {
        oldCtr match {
            case x if (x == ((1 << ctrBits) - 1) && taken) => 7
            case y if (y == 0 && !taken) => 0
            case _ => if (taken) {oldCtr + 1} else {oldCtr - 1}
        }
    }
    def log2(x: Int): Int = (log(x) / log(2)).toInt
    def getMask(len: Int) = (1 << len) - 1
    def getBit(x: Long, pos: Int): Int = (x & (1 << pos)).toInt
    // from 1 to 3: get bits (1,2,3)
    def getBits(x: Long, from: Int, to: Int): Int = ((x & (((1 << (to - from + 1)) - 1) << from)) >>> from).toInt
    def divUp(x: Int, y: Int): Int = (x / y) + (if (x % y != 0) 1 else 0)

    def boolArrayToLong(arr: Array[Boolean]): Long = arr.zipWithIndex.map{ case (b, i) => (if (b) 1 else 0) << i }.reduce(_|_)
    def boolArrayToInt(arr: Array[Boolean]): Int = boolArrayToLong(arr).toInt
    def toBoolArray(x: Long, len: Int): Array[Boolean] = {
        val arr = new Array[Boolean](len)
        (0 until len).foreach(i => arr(i) = (x >>> i) % 2 != 0)
        arr
    }
    def toBoolArray(x: Int, len: Int): Array[Boolean] = {
        val arr = new Array[Boolean](len)
        (0 until len).foreach(i => arr(i) = (x >>> i) % 2 != 0)
        arr
    }
    def PriorityEncoder(arr: Array[Boolean]): Int = {var res = arr.size - 1; arr.zipWithIndex.reverse.foreach{case(b,i) => if (b) res = i}; res}
    def PriorityEncoder(x: Int, len: Int): Int = PriorityEncoder(toBoolArray(x, len))
}

trait GTimer {
    var cycle: Long = 0
    def step(x: Long) = cycle += x
    def isCycle(x: Long): Boolean = cycle == x
}

class GlobalHistory(val maxlen: Int) {
    val hist: Array[Boolean] = new Array[Boolean](maxlen)
    var ptr: Int = 0
    def getHistPtr = this.ptr
    def getHist(len: Int = 64, ptr: Int = this.ptr): Array[Boolean] = {
        if (ptr - len >= 0)
            hist.slice(ptr-len, ptr)
        else
            hist.slice(ptr-len+maxlen, maxlen) ++ hist.slice(0, ptr)
    }
    def updateHist(taken: Boolean) = {
        hist.update(ptr, taken)
        ptr = if (ptr + 1 >= maxlen) ptr + 1 - maxlen else ptr + 1
    }
    def recover(oldPtr: Int, taken: Boolean) = {
        ptr = oldPtr
        updateHist(taken)
    }
}

class TableResp (val ctr: Int, val u: Int, val hit: Boolean) {}

class TageTable (val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int) extends TageParams with GTimer with Utils{
    val tagMask = getMask(tagLen)
    val ctrMask = getMask(3)
    val rowMask = getMask(log2(nRows))
    val bankMask = getMask(log2(TageBanks))

    class Entry () {
        var valid: Boolean = false
        var tag: Int = 0
        var ctr: Int = 0
        var u: Int = 0
        def this(tag: Int, ctr: Int, u: Int) = {
            this()
            this.valid = true
            this.tag = tag & tagMask
            this.ctr = ctr & ctrMask
            this.u = u
        }
    }

    val banks: Array[Array[Entry]] = Array.fill[Array[Entry]](TageBanks)(Array.fill[Entry](nRows)(new Entry))

    def ctrUpdate(oldCtr: Int, taken: Boolean): Int = satUpdate(taken, oldCtr, 3)

    def getBank(pc: Long): Int = getBits(pc, 1, log2(TageBanks))
    def getUnhashedIdx(pc: Long) = pc >> (1 + log2(TageBanks))
    def foldHist(hist: Long, len: Int) = (0 until divUp(histLen, len)).map(i => (hist >> (i*len)) & getMask(len)).reduce(_^_)
    def getIdx(unhashed: Long, hist: Long) = ((unhashed ^ foldHist(hist, log2(nRows))) & rowMask).toInt
    def getTag(unhashed: Long, hist: Long) = ((unhashed ^ foldHist(hist, tagLen)) & tagMask).toInt

    // Superscalar lookup
    def lookUp(pc: Long, hist: Long, mask: Array[Boolean]): Array[TableResp] = {
        val baseBank = getBank(pc)
        val unhashed = (0 until TageBanks).map(b => getUnhashedIdx(pc + 2 * b))
        val entries = (0 until TageBanks).map(b => banks((baseBank+b)&bankMask)(getIdx(unhashed(b), hist)))
        val tags = (0 until TageBanks).map(b => getTag(unhashed(b), hist))
        entries.zipWithIndex.map {case(e, i) => new TableResp(e.ctr, e.u, tags(i) == e.tag && e.valid && mask(i))}.toArray
    }

    // Per-bank lookup
    def lookUp(pc: Long, hist: Long): TableResp = {
        val bank = getBank(pc)
        lookUp(pc, hist, (0 until TageBanks).map(_ == 0).toArray)(0)
    }

    def update(pc: Long, hist: Long, valid: Boolean, taken: Boolean,
        alloc: Boolean, oldCtr: Int, uValid: Boolean, u: Int) = {
        val bank     = getBank(pc)
        val unhashed = getUnhashedIdx(pc)
        val idx      = getIdx(unhashed, hist)
        val tag      = getTag(unhashed, hist)

        if (valid) {
            banks(bank)(idx).valid = true
            banks(bank)(idx).tag = tag
            val newCtr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
            banks(bank)(idx).ctr = newCtr
            // printf(f"updating tag of bank $bank%d, idx $idx%d to $tag%x\n")
            // printf(f"updating ctr of bank $bank%d, idx $idx%d to $newCtr%d\n")
        }

        if (uValid) {
            banks(bank)(idx).u = u
            // printf(f"updating u of bank $bank%d, idx $idx%d to $u%d\n")
        }
    }

    def pvdrUpdate(pc: Long, hist: Long, taken: Boolean, oldCtr: Int, u: Int) = {
        update(pc, hist, true, taken, false, oldCtr, true, u)
    }
    def allocUpdate(pc: Long, hist: Long, taken: Boolean) = {
        update(pc, hist, true, taken, true, 0, true, 0)
    }
    def decrementU(pc: Long, hist: Long) = {
        update(pc, hist, false, false, false, 0, true, 0)
    }
}

class Bim (val nEntries: Int) extends Utils{
    val table = Array.fill[Int](nEntries)(2)
    val ctrBits = 2
    val idxMask = getMask(log2(nEntries))
    def getIdx(pc: Long): Int = (pc & idxMask).toInt
    def ctrUpdate(old: Int, taken: Boolean): Int = satUpdate(taken, old, ctrBits)
    def lookUp(pc: Long): Int = table(getIdx(pc))
    def update(pc: Long, taken: Boolean): Unit = {
        val newCtr = ctrUpdate(lookUp(pc), taken)
        val idx = getIdx(pc)
        table.update(idx, newCtr)
        // printf(f"bim updating idx $idx%d to $newCtr%d\n")
    }
}

class Tage extends TageParams with Utils{
    val speculativeUpdate = false
    val instantUpdate = true

    var brCount = 0

    val bim = new Bim(BimEntries)
    val tables = TableInfo.map {case(nRows, histLen, tagLen) => new TageTable(nRows, histLen, tagLen, UBitPeriod) }
    val ghist = new GlobalHistory(maxlen = 128)

    

    class TageMeta(val pc: Long, val histPtr: Int, val pvdr: Int, val pvdrValid: Boolean,
        val altDiff: Boolean, val pvdrU: Int, val pvdrCtr: Int, val alloc: Int,
        val allocValid: Boolean) {
        override def toString: String = {
            f"pc: 0x$pc%x, pvdr($pvdrValid%b): $pvdr%d, altDiff: $altDiff%b, pvdrU: $pvdrU%d, pvdrCtr: $pvdrCtr%d, alloc($allocValid%b): $alloc%d"
        }
    }

    val predictMetas = new mutable.Queue[TageMeta]

    def predict(pc: Long): Boolean = {
        // printf("predicting pc: 0x%x\n", pc)
        val tableResps = tables.map(t => {
            val hist = boolArrayToLong(ghist.getHist(t.histLen))
            t.lookUp(pc, hist)
        })
        val bimResp: Boolean = bim.lookUp(pc) >= 2
        // printf(f"bimResp: $bimResp%b\n")
        var altPred: Boolean = bimResp
        var provided: Boolean = false
        var provider = 0
        var res: Boolean = bimResp
        var finalAltPred: Boolean = bimResp
        for (i <- 0 until TageNTables) {
            val hit = tableResps(i).hit
            val ctr = tableResps(i).ctr
            if (hit) {res = if (ctr == 3 || ctr == 4) altPred else ctr >= 4}
            finalAltPred = if (hit) altPred else finalAltPred
            provided ||= hit
            provider = if (hit) i else provider
            altPred = if (hit) ctr >= 4 else altPred
        }
        val allocatableArray = tableResps.zipWithIndex.map{ case(r, i) => !r.hit && r.u == 0 && i > provider }.toArray
        val allocatable = boolArrayToInt(allocatableArray)
        val allocMask   = scala.util.Random.nextInt((1 << TageNTables))
        val maskedEntry = PriorityEncoder(allocatable & allocMask, TageNTables)
        val firstEntry  = PriorityEncoder(allocatable, TageNTables)
        val allocEntry  = if (allocatableArray(maskedEntry)) maskedEntry else firstEntry
        val meta = new TageMeta(pc, ghist.getHistPtr, provider, 
            provided, res != finalAltPred, tableResps(provider).u,
            tableResps(provider).ctr, allocEntry, allocatable != 0)
        predictMetas.enqueue(meta)
        brCount += 1
        println(meta)
        printf(f"pc:0x$pc%x predicted to be ${if (res) "taken" else "not taken"}%s\n")
        res
    }

    def update(pc: Long, taken: Boolean, pred: Boolean) = {
        printf(f"updating pc:0x$pc%x, taken:$taken%b, pred:$pred%b\n")
        val meta = predictMetas.dequeue()
        if (pc != meta.pc) println("update pc does not correspond with expected pc\n")
        
        bim.update(pc, taken)

        val misPred = taken != pred
        val hist = boolArrayToLong(ghist.getHist(ptr=meta.histPtr))
        if (meta.pvdrValid) {
            tables(meta.pvdr).pvdrUpdate(pc, hist, taken, meta.pvdrCtr, 
                if(meta.altDiff) satUpdate(!misPred, meta.pvdrU, 2) else meta.pvdrU)
        }
        if (misPred) {
            if (meta.allocValid) tables(meta.alloc).allocUpdate(pc, hist, taken)
            else (0 until TageNTables).foreach(i => if (i > meta.pvdr) tables(i).decrementU(pc, hist))
        }
    }
}

object TageTest {
    def main(args: Array[String]): Unit = {
        val bpd = new Tage
        val pred = bpd.predict(0x80000000L)
        bpd.update(0x80000000L, true, pred)
    }
}
