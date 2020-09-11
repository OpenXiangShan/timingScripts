package scalaTage

import scala.collection.mutable
// import scala.collection.immutable._
import scala.math._
import scala.util._


case class TageParams (
    val BimEntries: Int = 4096,
    val BimRatio: Int = 2,
    val minHist: Int = 4,
    val maxHist: Int = 640,
    //                                    Sets  Tag
    val t: Seq[Tuple2[Int, Int]]  = Seq(( 1024,   7),
                                        ( 1024,   7),
                                        ( 2048,   8),
                                        ( 2048,   8),
                                        ( 2048,   9),
                                        ( 2048,  10),
                                        ( 1024,  11),
                                        ( 1024,  12),
                                        ( 1024,  12),
                                        ( 1024,  13),
                                        (  512,  14),
                                        (  512,  15)),
    val TageBanks: Int = 16, // FetchWidth
    val UBitPeriod: Int = 4096)
{
    val TableInfo = t.zipWithIndex.map { case((s, ta), i) => {
        val hist = (minHist * pow(maxHist/minHist, i.toDouble / (t.size - 1)) + 0.5).toInt
        (s, hist, ta)
    }}

    val TageNTables = TableInfo.size

    val TotalBits: Int = TableInfo.map {
        case (s, h, t) => {
        s * (1+t+3+2)
        }
    }.reduce(_+_) + BimEntries + BimEntries / (1 << BimRatio)

    def outPutParams: String = TableInfo.zipWithIndex.map {case((r, h, t),i) => {f"TageTable[$i%d]: $r%3d rows, $h%3d bits history, $t%2d bits tag\n"}}.reduce(_+_) +
        f"bim: $BimEntries%d entries\n" +
        f"UBitPeriod: $UBitPeriod%d\n" +
        f"Totally consumed ${TotalBits/8192}%dKB"
}

abstract class TageComponents()(implicit params: TageParams) extends PredictorComponents {}

class TageTable (val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int, val i: Int)(implicit val p: TageParams) extends TageComponents()(p) {
    val useGem5 = true
    
    
    val tagMask = getMask(tagLen)
    val ctrMask = getMask(3)
    val rowMask = getMask(log2Up(nRows))
    val bankMask = getMask(log2Up(p.TageBanks))

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

    // val banks: Array[Array[Entry]] = Array.fill[Array[Entry]](TageBanks)(Array.fill[Entry](nRows)(new Entry))
    val bank: Array[Entry] = Array.fill[Entry](nRows)(new Entry)
    val idxHist = new FoldedHist(histLen, log2(nRows))
    val tagHist = (0 to 1).map(i => new FoldedHist(histLen, tagLen-i))
    val phist = new PathHistory(30, 2)

    // def flush = {banks.foreach(b => b.foreach(e => e.valid = false))}
    def flush = bank.foreach(e => e.valid = false)

    def ctrUpdate(oldCtr: Int, taken: Boolean): Int = satUpdate(taken, oldCtr, 3)

    def getBank(pc: Long): Int = ((pc >>> 1) & bankMask).toInt
    def getUnhashedIdx(pc: Long): Long = pc >>> (1 + log2(p.TageBanks))

    def F(phist: Int, size: Int): Int = {
        var a1: Int = 0
        var a2: Int = 0

        val masked = phist & getMask(size)
        a1 = masked & rowMask
        a2 = masked >>> log2Up(nRows)
        a2 = (a2 << i) & rowMask + (a2 >> (log2Up(nRows) - i))
        val a3 = a1 ^ a2
        val res = ((a3 << i) & rowMask) + (a3 >> (log2Up(nRows) - i))
        res
    }

    def getIdx(pc: Long): Int = 
        if (useGem5)
            (((pc >>> 1) ^ ((pc >>> 1) >>> (abs(log2Up(nRows)-i)+2)) ^ idxHist() ^ F(phist(), if (histLen > 16) 16 else histLen)) & rowMask).toInt
        else
            ((getUnhashedIdx(pc) ^ idxHist() ^ phist()) & rowMask).toInt
    
    def getTag(pc: Long): Int = 
        if (useGem5)
            (((pc >>> 1) ^ tagHist(0)() ^ (tagHist(1)() << 1)) & tagMask).toInt
        else
            (((getUnhashedIdx(pc) >>> log2(nRows)) ^ tagHist(0)() ^ (tagHist(1)() << 1)) & tagMask).toInt

    def getBIT(pc: Long): (Int, Int, Int) = (getBank(pc), getIdx(pc), getTag(pc))

    // def lookUp(pc: Long, mask: Array[Boolean]): Array[TableResp] = {
    //     val bits = (0 until TageBanks).map(b => getBIT(pc + 2*b))
    //     val (entries, tags) = bits.map { case (b, i, t) => {/*println(f"b:$b%d, i:$i%d, t$t%d"); */(banks(b)(i), t);} }.unzip
    //     (entries, tags, mask).zipped.map { case(e, t, m) => new TableResp(e.ctr, e.u, t == e.tag && e.valid && m)}.toArray
    // }

    // Per-bank lookup
    // def lookUp(pc: Long): TableResp = {
    //     lookUp(pc, (0 until TageBanks).map(_ == 0).toArray)(0) // the first bank is the target bank
    // }

    def lookUp(pc: Long): TableResp = {
        val idx = getIdx(pc)
        val tag = getTag(pc)
        val e = bank(idx)
        new TableResp(e.ctr, e.u, tag == e.tag && e.valid)
    }

    // def update(pc: Long, valid: Boolean, taken: Boolean,
    //     alloc: Boolean, oldCtr: Int, uValid: Boolean, u: Int) = {
    //     val (bank, idx, tag) = getBIT(pc)

    //     if (valid) {
    //         banks(bank)(idx).valid = true
    //         banks(bank)(idx).tag = tag
    //         banks(bank)(idx).ctr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
    //         // printf(f"updating tag of bank $bank%d, idx $idx%d to $tag%x\n")
    //         // printf(f"updating ctr of bank $bank%d, idx $idx%d to $newCtr%d\n")
    //     }

    //     if (uValid) {
    //         banks(bank)(idx).u = u
    //         // printf(f"updating u of bank $bank%d, idx $idx%d to $u%d\n")
    //     }
    // }

    def update(pc: Long, valid: Boolean, taken: Boolean,
        alloc: Boolean, oldCtr: Int, uValid: Boolean, u: Int) = {
        val idx = getIdx(pc)
        val tag = getTag(pc)

        if (valid) {
            bank(idx).valid = true
            bank(idx).tag = tag
            bank(idx).ctr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
            // printf(f"updating tag of bank $bank%d, idx $idx%d to $tag%x\n")
            // printf(f"updating ctr of bank $bank%d, idx $idx%d to $newCtr%d\n")
        }

        if (uValid) {
            bank(idx).u = u
            // printf(f"updating u of bank $bank%d, idx $idx%d to $u%d\n")
        }
    }


    def updateFoldedHistories(hNew: Boolean, hOld: Boolean) = {
        idxHist.update(hNew, hOld)
        tagHist.foreach(_.update(hNew, hOld))
    }

    def updatePathHistory(pc: Long) = phist.update(pc)

    def pvdrUpdate(pc: Long, taken: Boolean, oldCtr: Int, u: Int) = {
        update(pc, true, taken, false, oldCtr, true, u)
    }

    def allocUpdate(pc: Long, taken: Boolean) = {
        update(pc, true, taken, true, 0, true, 0)
    }

    // def decrementU(pc: Long) = {
    //     val (b, i, t) = getBIT(pc)
    //     banks(b)(i).decrementU
    // }
    
    def decrementU(pc: Long) = {
        val idx = getIdx(pc)
        bank(i).decrementU
    }
}

class Bim (val nEntries: Int, val ratio: Int)(implicit val p: TageParams) extends TageComponents()(p) {
    val predTable = Array.fill[Boolean](nEntries)(true)
    val hystTable = Array.fill[Boolean](nEntries >> ratio)(false)
    val ctrBits = 2
    val idxMask = getMask(log2Up(nEntries))
    def flush = {
        (0 until nEntries).foreach(predTable.update(_, true))
        (0 until nEntries >> ratio).foreach(hystTable.update(_, false))
    }
    def getIdx(pc: Long): (Int, Int) = {
        val pind = ((pc >>> 1) & idxMask).toInt
        (pind, pind >>> ratio)
    }
    def toInt(b: Boolean): Int = if (b) 1 else 0
    def toBool(i: Int): Boolean = if (i > 0) true else false
    def getCtr(ind: (Int, Int)): Int = (toInt(predTable(ind._1)) << 1) + toInt(hystTable(ind._2))
    def ctrUpdate(old: Int, taken: Boolean): Int = satUpdate(taken, old, ctrBits)
    def lookUp(pc: Long): Boolean = predTable(getIdx(pc)._1)
    def update(pc: Long, taken: Boolean): Unit = {
        val ind    = getIdx(pc)
        val oldCtr = getCtr(ind)
        val newCtr = ctrUpdate(oldCtr, taken)
        predTable.update(ind._1, toBool(newCtr & 0x2))
        hystTable.update(ind._2, toBool(newCtr & 0x1))
        // printf(f"bim updating idx $idx%d to $newCtr%d\n")
    }
}

class TableResp (val ctr: Int, val u: Int, val hit: Boolean) {}

class Tage(params: TageParams = TageParams()) extends BasePredictor {

    val speculativeUpdate = false
    val instantUpdate = true

    implicit val p: TageParams = params

    val TageNTables  = params.TageNTables
    val UBitPeriod   = params.UBitPeriod
    val maxHist      = params.maxHist
    val TableInfo    = params.TableInfo
    val outPutParams = params.outPutParams
    val BimEntries   = params.BimEntries
    val BimRatio     = params.BimRatio


    var brCount = 0


    val bim = new Bim(BimEntries, BimRatio)

    val tables = TableInfo.zipWithIndex.map { case((nRows, histLen, tagLen), i) => new TageTable(nRows, histLen, tagLen, UBitPeriod, i) }
    val ghist = new GlobalHistory(maxHist)


    class TageMeta(val pc: Long, val histPtr: Int, val pvdr: Int, val pvdrValid: Boolean,
        val altDiff: Boolean, val pvdrU: Int, val pvdrCtr: Int, val alloc: Int,
        val allocValid: Boolean) {
        override def toString: String = {
            f"pc: 0x$pc%x, histPtr:${histPtr}%3d, pvdr($pvdrValid%5b): $pvdr%d, altDiff: $altDiff%5b, pvdrU: $pvdrU%d, pvdrCtr: $pvdrCtr%d, alloc($allocValid%5b): $alloc%d"
        }
    }

    val predictMetas = new mutable.Queue[TageMeta]

    def predict(pc: Long) = true

    def predict(pc: Long, isBr: Boolean): Boolean = {
        // printf("predicting pc: 0x%x\n", pc)
        val nowPtr = ghist.getHistPtr
        val (meta, res) = 
            if (isBr) {
                val tableResps = tables.map(t => t.lookUp(pc))
                val bimResp: Boolean = bim.lookUp(pc)
                // printf(f"bimResp: $bimResp%b\n")

                def calRes(tResp: List[TableResp], ti: Int, provided: Boolean, provider: Int,
                    altPred: Boolean, finalAltPred: Boolean, res: Boolean): (Boolean, Int, Boolean, Boolean, Boolean) = {
                    val hit = tResp(0).hit
                    val ctr = tResp(0).ctr
                    val u   = tResp(0).u
                    tResp match {
                        case Nil => {println("Should not reach here"); (false, 0, false, false, false)}
                        case resp :: Nil => {
                            if (resp.hit) (true, ti, ctr >= 4, altPred, if ((ctr == 3 || ctr == 4) && u == 0) altPred else ctr >= 4)
                            else (provided, provider, altPred, finalAltPred, res)
                        }
                        case resp :: tail => {
                            if (resp.hit) calRes(tail, ti+1, true, ti, ctr >= 4, altPred, (if ((ctr == 3 || ctr == 4) && u == 0) altPred else (ctr >= 4)))
                            else calRes(tail, ti+1, provided, provider, altPred, finalAltPred, res)
                        }
                    }
                }
                
                val (provided, provider, altPred, finalAltPred, res) = calRes(tableResps.toList, 0, false, 0, bimResp, bimResp, bimResp)

                val allocatableArray = tableResps.zipWithIndex.map{ case(r, i) => !r.hit && r.u == 0 && ((i > provider && provided) || !provided) }.toArray
                val allocatable = boolArrayToInt(allocatableArray)
                val allocMask   = scala.util.Random.nextInt((1 << TageNTables))
                val maskedEntry = PriorityEncoder(allocatable & allocMask, TageNTables)
                val firstEntry  = PriorityEncoder(allocatable, TageNTables)
                val allocEntry  = if (allocatableArray(maskedEntry)) maskedEntry else firstEntry


                val altDiffers = res != finalAltPred
                Debug(f"pc:0x$pc%x predicted to be ${if (res) "taken" else "not taken"}%s")
                (new TageMeta(pc, nowPtr, provider, 
                    provided, altDiffers, tableResps(provider).u,
                    tableResps(provider).ctr, allocEntry, allocatable != 0), res)
            }
            else
                (new TageMeta(pc, nowPtr, 0, false, false, 0, 0, 0, false), true)

        if (isBr || updateOnUncond) predictMetas.enqueue(meta)
        if (isBr) brCount += 1
        // if (brCount % UBitPeriod == 0) tables.foreach(t => t.banks.foreach(b => b.foreach(e => e.decrementU )))
        if (brCount % UBitPeriod == 0) tables.foreach(t => t.bank.foreach(e => e.decrementU))
        res
    }
    

    def updateUncond(pc: Long) = {
        if (updateOnUncond) {
            val meta = predictMetas.dequeue()
            if (pc != meta.pc) Debug("update pc does not correspond with expected pc\n")
            ghist.updateHist(true)
            val oldBits = tables.map(t => ghist(meta.histPtr-t.histLen)).toArray
            tables.zip(oldBits).foreach{ case(t, o) => t.updateFoldedHistories(true, o) }
            tables.foreach(_.updatePathHistory(pc))
        }
    }

    def update(pc: Long, taken: Boolean, pred: Boolean) = {
        // printf(f"updating pc:0x$pc%x, taken:$taken%b, pred:$pred%b\n")
        val meta = predictMetas.dequeue()
        if (pc != meta.pc) Debug("update pc does not correspond with expected pc\n")
        val misPred = taken != pred

        Debug("[update meta] " + meta + f" | ${if (taken) " T" else "NT"}%s pred to ${if (pred) " T" else "NT"}%s -> ${if(misPred) "miss" else "corr"}%s")
        Debug(f"[update hist] ${ghist.getHistStr(ptr=meta.histPtr)}%s")

        ghist.updateHist(taken)
        bim.update(pc, taken)

        if (meta.pvdrValid) {
            tables(meta.pvdr).pvdrUpdate(pc, taken, meta.pvdrCtr, 
                if(meta.altDiff) satUpdate(!misPred, meta.pvdrU, 2) else meta.pvdrU)
        }
        if (misPred) {
            if (meta.allocValid) tables(meta.alloc).allocUpdate(pc, taken)
            else (0 until TageNTables).foreach(i => if (i > meta.pvdr) tables(i).decrementU(pc))
        }
        val oldBits = tables.map(t => ghist(meta.histPtr-t.histLen)).toArray
        tables.zip(oldBits).foreach{ case(t, o) => t.updateFoldedHistories(taken, o) }
        tables.foreach(_.updatePathHistory(pc))
    }

    def flush() = {
        bim.flush
        tables.foreach(_.flush)
    }

    def name: String = "BOOM_TAGE"
    override def toString: String = f"${this.name}%s with params:\n${outPutParams}%s\nUsing global history ${if(updateOnUncond) "with" else "without"}%s jumps\n"
}

object Tage {
    def apply(ops: Map[Symbol, Any]): Tage = {
        if (ops.contains('hislen)) {
            println(f"max history length set to ${ops('hislen).asInstanceOf[Int]}%d")
            new Tage(TageParams(maxHist=ops('hislen).asInstanceOf[Int]))
        }
        else
            new Tage
    }
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
