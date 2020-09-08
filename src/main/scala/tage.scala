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
                        // ( 128,  128,   10))
                        // ( 128,  256,   10))
    def maxHisLen: Int = TableInfo.last._2
    val TageNTables = TableInfo.size
    val UBitPeriod = 4096
    val TageBanks = 16 // FetchWidth

    val TotalBits: Int = TableInfo.map {
        case (s, h, t) => {
        s * (1+t+3+2) * TageBanks
        }
    }.reduce(_+_) + BimEntries * 2

    def outPutParams: String = TableInfo.zipWithIndex.map {case((r, h, t),i) => {f"TageTable[$i%d]: $r%3d rows, $h%3d bits history, $t%2d bits tag\n"}}.reduce(_+_) +
        f"bim: $BimEntries%d entries\n" +
        f"UBitPeriod: $UBitPeriod%d\n" +
        f"Totally consumed ${TotalBits/8192}%dKB"
}

abstract class TageComponents extends PredictorComponents with TageParams {}

class TableResp (val ctr: Int, val u: Int, val hit: Boolean) {}

class TageTable (val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int) extends TageComponents {
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
        def decrementU = this.u = satUpdate(false, this.u, 2)
    }

    val banks: Array[Array[Entry]] = Array.fill[Array[Entry]](TageBanks)(Array.fill[Entry](nRows)(new Entry))

    def flush = {banks.foreach(b => b.foreach(e => e.valid = false))}

    def ctrUpdate(oldCtr: Int, taken: Boolean): Int = satUpdate(taken, oldCtr, 3)

    def getBank(pc: Long): Int = ((pc >>> 1) & bankMask).toInt
    def getUnhashedIdx(pc: Long): Long = pc >>> 1
    def foldHist(hist: Array[Boolean], len: Int): Long = (0 until divUp(histLen, len)).map(i => boolArrayToLong(hist.reverse.slice(i*len, min((i+1)*len, histLen)))).reduce(_^_)
    def getIdx(unhashed: Long, hist: Array[Boolean]): Int = ((unhashed ^ foldHist(hist, log2(nRows))) & rowMask).toInt
    def getTag(unhashed: Long, hist: Array[Boolean]): Int = (((unhashed >>> log2(nRows)) ^ foldHist(hist, tagLen)) & tagMask).toInt

    def getBIT(pc: Long, hist: Array[Boolean]): (Int, Int, Int) = (getBank(pc), getIdx(getUnhashedIdx(pc), hist), getTag(getUnhashedIdx(pc), hist))

    // Superscalar lookup
    def lookUp(pc: Long, hist: Array[Boolean], mask: Array[Boolean]): Array[TableResp] = {
        val bits = (0 until TageBanks).map(b => getBIT(pc + 2*b, hist))
        val entries = bits.map { case (b, i, t) => banks(b)(i) }
        val tags    = bits.map { case (b, i, t) => t }
        (entries, tags, mask).zipped.map { case(e, t, m) => new TableResp(e.ctr, e.u, t == e.tag && e.valid && m)}.toArray
    }

    // Per-bank lookup
    def lookUp(pc: Long, hist: Array[Boolean]): TableResp = {
        lookUp(pc, hist, (0 until TageBanks).map(_ == 0).toArray)(0) // the first bank is the target bank
    }

    def update(pc: Long, hist: Array[Boolean], valid: Boolean, taken: Boolean,
        alloc: Boolean, oldCtr: Int, uValid: Boolean, u: Int) = {
        val (bank, idx, tag) = getBIT(pc, hist)

        if (valid) {
            banks(bank)(idx).valid = true
            banks(bank)(idx).tag = tag
            banks(bank)(idx).ctr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
            // printf(f"updating tag of bank $bank%d, idx $idx%d to $tag%x\n")
            // printf(f"updating ctr of bank $bank%d, idx $idx%d to $newCtr%d\n")
        }

        if (uValid) {
            banks(bank)(idx).u = u
            // printf(f"updating u of bank $bank%d, idx $idx%d to $u%d\n")
        }
    }

    def pvdrUpdate(pc: Long, hist: Array[Boolean], taken: Boolean, oldCtr: Int, u: Int) = {
        update(pc, hist, true, taken, false, oldCtr, true, u)
    }
    def allocUpdate(pc: Long, hist: Array[Boolean], taken: Boolean) = {
        update(pc, hist, true, taken, true, 0, true, 0)
    }
    def decrementU(pc: Long, hist: Array[Boolean]) = {
        val (b, i, t) = getBIT(pc, hist)
        banks(b)(i).decrementU
    }
}

class Bim () extends TageComponents {
    val nEntries = BimEntries
    val table = Array.fill[Int](nEntries)(2)
    val ctrBits = 2
    val idxMask = getMask(log2Up(nEntries))
    def flush = (0 until nEntries).foreach(table.update(_, 2))
    def getIdx(pc: Long): Int = ((pc >>> 1) & idxMask).toInt
    def ctrUpdate(old: Int, taken: Boolean): Int = satUpdate(taken, old, ctrBits)
    def lookUp(pc: Long): Int = table(getIdx(pc))
    def update(pc: Long, taken: Boolean): Unit = {
        table.update(getIdx(pc), ctrUpdate(lookUp(pc), taken))
        // printf(f"bim updating idx $idx%d to $newCtr%d\n")
    }
}

class Tage extends BasePredictor with TageParams {

    val speculativeUpdate = false
    val instantUpdate = true

    var brCount = 0

    val bim = new Bim()
    val tables = TableInfo.map { case(nRows, histLen, tagLen) => new TageTable(nRows, histLen, tagLen, UBitPeriod) }
    val ghist = new GlobalHistory(maxHisLen)

    

    class TageMeta(val pc: Long, val histPtr: Int, val pvdr: Int, val pvdrValid: Boolean,
        val altDiff: Boolean, val pvdrU: Int, val pvdrCtr: Int, val alloc: Int,
        val allocValid: Boolean, val hist: Array[Boolean], val allocatable: Array[Boolean],
        val maskedEntry: Int, val firstEntry: Int) {
        override def toString: String = {
            f"pc: 0x$pc%x, pvdr($pvdrValid%5b): $pvdr%d, altDiff: $altDiff%5b, pvdrU: $pvdrU%d, pvdrCtr: $pvdrCtr%d, alloc($allocValid%5b): $alloc%d in ${boolArrayToString(allocatable)}%s(masked:$maskedEntry%d, first:$firstEntry%d), hist: ${boolArrayToString(hist)}%s"
        }
    }

    val predictMetas = new mutable.Queue[TageMeta]

    def predict(pc: Long): Boolean = {
        // printf("predicting pc: 0x%x\n", pc)
        val hist = ghist.getHist(maxHisLen)
        val tableResps = tables.map(t => t.lookUp(pc, hist))
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
            val u   = tableResps(i).u
            if (hit) {
                res = if ((ctr == 3 || ctr == 4) && u == 0) altPred else ctr >= 4
                finalAltPred = altPred
                provider = i
                altPred = ctr >= 4
            }
            provided ||= hit
        }
        val allocatableArray = tableResps.zipWithIndex.map{ case(r, i) => !r.hit && r.u == 0 && ((i > provider && provided) || !provided) }.toArray
        val allocatable = boolArrayToInt(allocatableArray)
        val allocMask   = scala.util.Random.nextInt((1 << TageNTables))
        // val allocMask   = (1 << TageNTables) - 1
        val maskedEntry = PriorityEncoder(allocatable & allocMask, TageNTables)
        val firstEntry  = PriorityEncoder(allocatable, TageNTables)
        val allocEntry  = if (allocatableArray(maskedEntry)) maskedEntry else firstEntry

        val meta = new TageMeta(pc, ghist.getHistPtr, provider, 
            provided, res != finalAltPred, tableResps(provider).u,
            tableResps(provider).ctr, allocEntry, allocatable != 0,
            hist, allocatableArray, maskedEntry, firstEntry)
        predictMetas.enqueue(meta)
        brCount += 1
        if (brCount % UBitPeriod == 0) tables.foreach(t => t.banks.foreach(b => b.foreach(e => e.decrementU )))

        Debug(f"pc:0x$pc%x predicted to be ${if (res) "taken" else "not taken"}%s")
        
        res
    }

    def update(pc: Long, taken: Boolean, pred: Boolean) = {
        // printf(f"updating pc:0x$pc%x, taken:$taken%b, pred:$pred%b\n")
        val meta = predictMetas.dequeue()
        if (pc != meta.pc) Debug("update pc does not correspond with expected pc\n")
        bim.update(pc, taken)
        val misPred = taken != pred

        Debug("[update meta] " + meta + f" | ${if (taken) " T" else "NT"}%s pred to ${if (pred) " T" else "NT"}%s -> ${if(misPred) "miss" else "corr"}%s")
        Debug(f"[update hist] ${ghist.getHistStr(ptr=meta.histPtr)}%s")

        ghist.updateHist(taken)
        val hist = ghist.getHist(ptr=meta.histPtr)
        if (meta.pvdrValid) {
            tables(meta.pvdr).pvdrUpdate(pc, hist, taken, meta.pvdrCtr, 
                if(meta.altDiff) satUpdate(!misPred, meta.pvdrU, 2) else meta.pvdrU)
        }
        if (misPred) {
            if (meta.allocValid) tables(meta.alloc).allocUpdate(pc, hist, taken)
            else (0 until TageNTables).foreach(i => if (i > meta.pvdr) tables(i).decrementU(pc, hist))
        }
    }

    def flush() = {
        bim.flush
        tables.foreach(_.flush)
    }

    def name: String = "BOOM_TAGE"
    override def toString: String = f"${this.name}%s with params:\n${outPutParams}%s\n"
}

// object TageTest extends PredictorUtils{
//     def main(args: Array[String]): Unit = {
//         val bpd = new Tage
//         var hist: Long = 0
//         for (i <- 0 until 10) {
//             // val pred = bpd.predict(0x80000000L)
//             val taken = i % 2 == 0
//             bpd.ghist.updateHist(taken)
//             hist = (hist << 1) | (if (taken) 1 else 0)
//             val tageHist = boolArrayToLong(bpd.ghist.getHist(len=64))
//             var wrong = false
//             if (tageHist != hist) {
//                 println(f"at br$i%d, ref_hist:$hist%x, tageHist:$tageHist%x, ptr:${bpd.ghist.getHistPtr}")
//                 wrong = true
//             }
//             if (wrong) println("Wrong!")
//         }
//     }
// }
