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
    val UBitPeriod: Int = 4096,
    val SuperScalar: Boolean = false,
    val useGem5: Boolean = false,
    val useStatisticalCorrector: Boolean = false)
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

    override def toString: String = TableInfo.zipWithIndex.map {case((r, h, t),i) => {f"TageTable[$i%d]: $r%3d rows, $h%3d bits history, $t%2d bits tag\n"}}.reduce(_+_) +
        f"bim: $BimEntries%d entries\n" +
        f"UBitPeriod: $UBitPeriod%d\n" +
        f"Totally consumed ${TotalBits/8192}%dKB"
}

abstract class TageComponents()(implicit params: TageParams) extends PredictorComponents {}

class SCTable(val numRows: Int, val ctrBits: Int, val histLen: Int) extends PredictorComponents {
    lazy val scTable = Array.fill[Array[SatCounter]](numRows)(Array.fill[SatCounter](2)(SatCounter(ctrBits, 0, signed=true)))
    val rowMask = getMask(log2Up(numRows))
    def getIdx(pc: Long, hist: Int): Int = ((pc >>> 1) & rowMask).toInt ^ hist
    def lookup(pc: Long, hist: Int): List[Int] = {
        val idx = getIdx(pc, hist)
        scTable(idx).map(_()).toList
    }
    def update(idx: Int, tageTaken: Boolean, predCorr: Boolean): Unit = {
        val subIdx = if (tageTaken) 1 else 0
        scTable(idx)(subIdx) = scTable(idx)(subIdx).update(predCorr)
    }
    def update(pc: Long, hist: Int, tageTaken: Boolean, predCorr: Boolean): Unit =
        update(getIdx(pc, hist), tageTaken, predCorr)
}

case class SCThreshold(val threshold: Int, val tc: SatCounter = SatCounter(6, 0, signed=true)) {
    def initialThres: Int = 10 // to be validated
    val tcBit = 6
    def apply() = this.threshold
    def update(cause: Boolean): SCThreshold = {
        val newTC = tc.update(cause)
        if (newTC.isSaturatedPos) this.copy(tc=SatCounter(tcBit, 0, signed=true), threshold=this.threshold+1)
        else if (newTC.isSaturatedNeg) this.copy(tc=SatCounter(tcBit, 0, signed=true), threshold=this.threshold-1)
        else this.copy(tc=newTC)
    }
}

class TageTable (val numRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int, val i: Int)(implicit val p: TageParams) extends TageComponents()(p) {
    val useGem5     = p.useGem5
    val SuperScalar = p.SuperScalar
    val TageBanks   = p.TageBanks
    val nRows = if (SuperScalar) numRows / TageBanks else numRows
    
    
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

    lazy val tables = Array.fill[Array[Entry]](TageBanks)(Array.fill[Entry](nRows)(new Entry))
    lazy val table  = Array.fill[Entry](nRows)(new Entry)
    println(f"size of table(${i}%d): ${table.size}%d")

    val idxHist = new FoldedHist(histLen, log2(nRows))
    val tagHist = (0 to 1).map(i => new FoldedHist(histLen, tagLen-i))
    val phist = new PathHistory(30, 2)

    private def ctrUpdate(oldCtr: Int, taken: Boolean): Int = satUpdate(taken, oldCtr, 3)

    private def getBank(pc: Long): Int = ((pc >>> 1) & bankMask).toInt
    private def getUnhashedIdx(pc: Long): Long = pc >>> (1 + log2(p.TageBanks))

    private def F(phist: Int, size: Int): Int = {
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

    private def getIdx(pc: Long, idxHist: Int, phist: Int): Int = 
        if (useGem5)
            (((pc >>> 1) ^ ((pc >>> 1) >>> (abs(log2Up(nRows)-i)+2)) ^ idxHist ^ F(phist, if (histLen > 16) 16 else histLen)) & rowMask).toInt
        else
            ((getUnhashedIdx(pc) ^ idxHist ^ phist) & rowMask).toInt
    
    private def getTag(pc: Long, tagHist: List[Int]): Int = {
        assert(tagHist.length == 2)
        if (useGem5)
            (((pc >>> 1) ^ tagHist(0) ^ (tagHist(1) << 1)) & tagMask).toInt
        else
            (((getUnhashedIdx(pc) >>> log2(nRows)) ^ tagHist(0) ^ (tagHist(1) << 1)) & tagMask).toInt
    }

    private def getBIT(pc: Long, idxHist: Int,
        tagHist: List[Int], phist: Int): (Int, Int, Int) = (getBank(pc), getIdx(pc, idxHist, phist), getTag(pc, tagHist))

    def flush = if (SuperScalar) tables.foreach(b => b.foreach(e => e.valid = false))
                else             table.foreach(e => e.valid = false)

    def lookUp(pc: Long, idxHist: Int, tagHist: List[Int], phist: Int): TableResp = {
        val (bank, idx, tag) = getBIT(pc, idxHist, tagHist, phist)
        val e = if (SuperScalar) tables(bank)(idx) else table(idx)
        TableResp(e.ctr, e.u, tag == e.tag && e.valid)
    }

    
    def update(pc: Long, valid: Boolean, taken: Boolean,
        alloc: Boolean, oldCtr: Int, uValid: Boolean, u: Int,
        fhist: List[Int], phist: Int) = {

        val idxHist = fhist(0)
        val tagHist = List(fhist(1), fhist(2))
        val (bank, idx, tag) = getBIT(pc, idxHist, tagHist, phist)

        if (valid) {
            if (SuperScalar) {
                tables(bank)(idx).valid = true
                tables(bank)(idx).tag = tag
                tables(bank)(idx).ctr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
            }
            else {
                table(idx).valid = true
                table(idx).tag = tag
                table(idx).ctr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
            }
            // printf(f"updating tag of bank $bank%d, idx $idx%d to $tag%x\n")
            // printf(f"updating ctr of bank $bank%d, idx $idx%d to $newCtr%d\n")
        }

        if (uValid) {
            if (SuperScalar) tables(bank)(idx).u = u
            else             table(idx).u = u
            // printf(f"updating u of bank $bank%d, idx $idx%d to $u%d\n")
        }
    }

    def pvdrUpdate(pc: Long, taken: Boolean, oldCtr: Int, u: Int, fhist: List[Int], phist: Int) = {
        update(pc, true, taken, false, oldCtr, true, u, fhist, phist)
    }

    def allocUpdate(pc: Long, taken: Boolean, fhist: List[Int], phist: Int) = {
        update(pc, true, taken, true, 0, true, 0, fhist, phist)
    }

    def decrementU(pc: Long, idxHist: Int, phist: Int) = {
        val bank = getBank(pc)
        val idx = getIdx(pc, idxHist, phist)
        if (SuperScalar) tables(bank)(i).decrementU
        else             table(i).decrementU
    }
}

class Bim (val nEntries: Int, val ratio: Int)(implicit val p: TageParams) extends TageComponents()(p) {
    val predTable = Array.fill[Boolean](nEntries)(false)
    val hystTable = Array.fill[Boolean](nEntries >> ratio)(true)
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

case class TableResp (val ctr: Int, val u: Int, val hit: Boolean) {}
case class TageHistories(val foldedHists: List[List[Int]], val pHist: Int) {}



class Tage(params: TageParams = TageParams()) extends BasePredictor {

    val instantUpdate = true

    implicit val p: TageParams = params

    val TageNTables  = params.TageNTables
    val UBitPeriod   = params.UBitPeriod
    val maxHist      = params.maxHist
    val TableInfo    = params.TableInfo
    val BimEntries   = params.BimEntries
    val BimRatio     = params.BimRatio
    val SuperScalar  = params.SuperScalar
    val TageBanks    = params.TageBanks
    val UseSC        = params.useStatisticalCorrector

    val scNumTables = 5
    // Use the same history length as TAGE tables
    val scHistLens = 0 :: TableInfo.take(scNumTables-1).map { case (_, h, _) => h}.toList
    val scTableInfos = List.fill[Tuple2[Int, Int]](scNumTables)((256, 6)) zip scHistLens map { case ((nRows, cBits), h) => (nRows, cBits, h) }
    var scThreshold = SCThreshold(10)

    var brCount = 0


    val bim = new Bim(BimEntries, BimRatio)

    val tables = TableInfo.zipWithIndex.map { case((nRows, histLen, tagLen), i) => new TageTable(nRows, histLen, tagLen, UBitPeriod, i) }
    val scTables = scTableInfos.map { case (nRows, cBits, h) => new SCTable(nRows, cBits, h)}
    val ghist = new GlobalHistory(maxHist)

    val tageIdxHists = TableInfo.map { case (nRows, h, tL) => new FoldedHist(h, log2(nRows))}
    val tageTagHistsTemp = TableInfo.map { case (nRows, h, tL) => (new FoldedHist(h, tL), new FoldedHist(h, tL-1))}.unzip
    val tageTagHists = List(tageTagHistsTemp._1.toList, tageTagHistsTemp._2.toList)
    // println(type(tageTagH))
    // val tageTagHists = tageTagH.unzip
    println(f"length tageTagHists ${tageTagHists(0).length}%d")
    val phist = new PathHistory(30, 2)
    lazy val scHists = scTableInfos.map { case (nRows, _, h) => new FoldedHist(h, log2Up(nRows))}.toList


    case class TageMeta(val hist: TageHistories, val pvdr: Int, val pvdrValid: Boolean,
        val altDiff: Boolean, val pvdrU: Int, val pvdrCtr: Int, val alloc: Int,
        val allocValid: Boolean) {
        override def toString: String = {
            f"pvdr($pvdrValid%5b): $pvdr%d, altDiff: $altDiff%5b, pvdrU: $pvdrU%d, pvdrCtr: $pvdrCtr%d, alloc($allocValid%5b): $alloc%d"
        }
    }
    
    case class SCMeta(val hist: List[Int], val tagePred: Boolean, val scUsed: Boolean,
        val scReverted: Boolean, val sumBelowThreshold: Boolean) {}

    case class TagePredictionMeta(val pc: Long, val histPtr: Int, val tageMeta: TageMeta, val scMeta: SCMeta,
        val pred: Boolean, val isBr: Boolean) extends PredictionMeta {}

    @scala.annotation.tailrec
    final def tageCalRes(tResp: List[TableResp], ti: Int, provided: Boolean, provider: Int,
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
                if (resp.hit) tageCalRes(tail, ti+1, true, ti, ctr >= 4, altPred, (if ((ctr == 3 || ctr == 4) && u == 0) altPred else (ctr >= 4)))
                else tageCalRes(tail, ti+1, provided, provider, altPred, finalAltPred, res)
            }
        }
    }

    val obq = new mutable.Queue[TagePredictionMeta]

    def predict(pc: Long) = true

    def predict(pc: Long, mask: Int): Boolean = true

    def predict(pc: Long, isBr: Boolean): Boolean = {
        // printf("predicting pc: 0x%x\n", pc)
        val nowPtr = ghist.getHistPtr
        val (meta, res) = 
            if (isBr) {
                val tageTableResps = tables.map {
                    case t => {
                        val i = t.i
                        t.lookUp(pc, tageIdxHists(i)(), List(tageTagHists(0)(i)(), tageTagHists(1)(i)()), phist())
                    }
                }
                val bimResp: Boolean = bim.lookUp(pc)

                val (provided, provider, altPred, finalAltPred, tageRes) = tageCalRes(tageTableResps.toList, 0, false, 0, bimResp, bimResp, bimResp)

                val scResps = (scTables zip scHists) map { case (t, h) => t.lookup(pc, h()) }
                val scs = scResps map { case List(rnt, rt) => if (tageRes) rt else rnt }
                val scCentered = scs map (r => 2 * r - 1)
                val scSum = scCentered reduce (_+_)
                val tagePredCenteredVal = 2 * (tageTableResps(provider).ctr - 4) - 1
                val totalSum = scSum + 8 * tagePredCenteredVal
                val sumBelowThreshold = abs(totalSum) < scThreshold()
                val scReverted = totalSum < 0 && sumBelowThreshold
                var res = tageRes
                if (UseSC && provided) {
                    // only when SC disagrees and sum is above threshold should we revert the prediction
                    if (scReverted) res = !res
                }
                
                if (res != tageRes) System.exit(0)

                val allocatableArray = tageTableResps.zipWithIndex.map{ case(r, i) => !r.hit && r.u == 0 && ((i > provider && provided) || !provided) }.toArray
                val allocatable = boolArrayToInt(allocatableArray)
                val allocMask   = scala.util.Random.nextInt((1 << TageNTables))
                val maskedEntry = PriorityEncoder(allocatable & allocMask, TageNTables)
                val firstEntry  = PriorityEncoder(allocatable, TageNTables)
                val allocEntry  = if (allocatableArray(maskedEntry)) maskedEntry else firstEntry


                val altDiffers = res != finalAltPred
                Debug(f"pc:0x$pc%x predicted to be ${if (res) "taken" else "not taken"}%s")
                val tageFolded = (tageIdxHists map (_.apply()),
                                  tageTagHists(0) map (_.apply()),
                                  tageTagHists(1) map (_.apply())).zipped.toList.map{case (a, b, c) => List(a, b, c)}.toList
                val tageHist = TageHistories(tageFolded, phist())
                val scHist = (scHists map {(i: FoldedHist) => i match {case t=> t.apply()}}).toList
                (TagePredictionMeta(pc, nowPtr,
                    TageMeta(tageHist, provider, provided, altDiffers, 
                    tageTableResps(provider).u, tageTableResps(provider).ctr,
                    allocEntry, allocatable != 0),
                    SCMeta(scHist, tageRes, provided, scReverted, sumBelowThreshold), res, isBr),
                 res)
            }
            else
                (TagePredictionMeta(pc, nowPtr,
                          TageMeta(TageHistories(List(List(0)), 0), 0, false, false, 0, 0, 0, false),
                          SCMeta(List(0), true, false, false, false),
                          true, isBr),
                 true)

        if (isBr || updateOnUncond) obq.enqueue(meta)
        if (isBr) brCount += 1
        if (brCount % UBitPeriod == 0) {
            if (SuperScalar) tables.foreach(t => t.tables.foreach(b => b.foreach(e => e.decrementU )))
            else             tables.foreach(t => t.table.foreach(e => e.decrementU))
        }

        res
    }

    def updateFoldedHistories(taken: Boolean, histPtr: Int) = {
        def updateWithOldBit(x: List[Tuple2[FoldedHist, Boolean]]) = 
            x.foreach { case (h, o) => h.update(taken, o)}

        val tageOldBits = tables.map(t => ghist(histPtr-t.histLen))
        updateWithOldBit((tageIdxHists, tageOldBits).zipped.toList)
        if (UseSC) {
            val scOldBits = scHistLens.map(l => ghist(histPtr-l))
            updateWithOldBit((scHists, scOldBits).zipped.toList)
        }
    }

    def updateUncond(pc: Long) = {
        if (updateOnUncond) {
            val meta = obq.dequeue()
            if (pc != meta.pc) Debug("update pc does not correspond with expected pc\n")
            ghist.updateHist(true)
            updateFoldedHistories(true, meta.histPtr)
            phist.update(pc)
        }
    }

    def update(pc: Long, taken: Boolean) = {
        // printf(f"updating pc:0x$pc%x, taken:$taken%b, pred:$pred%b\n")
        val meta = obq.head
        val pred = meta.pred
        if (pc != meta.pc) {
            println("update pc does not correspond with expected pc\n")
        } else {
            obq.dequeue()
            val misPred = taken != pred

            Debug("[update meta] " + meta + f" | ${if (taken) " T" else "NT"}%s pred to ${if (pred) " T" else "NT"}%s -> ${if(misPred) "miss" else "corr"}%s")
            Debug(f"[update hist] ${ghist.getHistStr(ptr=meta.histPtr)}%s")
            val tageMeta = meta.tageMeta
            val tageHists = tageMeta.hist
            if (tageMeta.pvdrValid) {
                tables(tageMeta.pvdr).pvdrUpdate(pc, taken, tageMeta.pvdrCtr, 
                    if(tageMeta.altDiff) satUpdate(!misPred, tageMeta.pvdrU, 2) else tageMeta.pvdrU,
                    tageHists.foldedHists(tageMeta.pvdr), tageHists.pHist)
            }
            if (misPred) {
                val idxHists = tageHists.foldedHists.map(h => h(0)).toList
                val pHist = tageHists.pHist
                if (tageMeta.allocValid) tables(tageMeta.alloc).allocUpdate(pc, taken, tageHists.foldedHists(tageMeta.pvdr), tageHists.pHist)
                else (0 until TageNTables).foreach(i => if (i > tageMeta.pvdr) tables(i).decrementU(pc, idxHists(i), pHist))
            }
            if (UseSC) {
                val scMeta = meta.scMeta
                val scHist = scMeta.hist
                if (scMeta.scUsed) {
                    if (misPred || scMeta.sumBelowThreshold) {
                        (scTables zip scHist) foreach { case (t, h) => t.update(pc, h, scMeta.tagePred, !misPred) }
                        // update on mispred has a higher priority
                        scThreshold = scThreshold.update(misPred)
                    }
                }
            }
            ghist.updateHist(taken)
            bim.update(pc, taken)
            updateFoldedHistories(taken, meta.histPtr)
            phist.update(pc)
        }
        pred
    }

    def flush() = {
        bim.flush
        tables.foreach(_.flush)
    }

    def name: String = "BOOM_TAGE"
    override def toString: String = f"${this.name}%s with params:\n${params}%s\nUsing global history ${if(updateOnUncond) "with" else "without"}%s jumps\n"
}

object Tage {
    // This is a factory method to produce Tage instances according to passed option
    def apply(ops: Map[Symbol, Any]): Tage = new Tage(wrapParams(ops, TageParams()))

    @scala.annotation.tailrec
    def wrapParams(ops: Map[Symbol, Any], p: TageParams = TageParams()): TageParams = 
        ops match {
            case o if (o.isEmpty) => p
            case o if (o.contains('hislen)) => {
                println(f"max history length set to ${ops('hislen).asInstanceOf[Int]}%d")
                wrapParams(ops - 'hislen, p.copy(maxHist=ops('hislen).asInstanceOf[Int]))
            }
            case o if (o.contains('superscalar)) => {
                println(f"tables set to superscalar")
                wrapParams(ops - 'superscalar, p.copy(SuperScalar=true))
            }
            case o if (o.contains('updateOnUncond)) => {
                println(f"updating history on unconditional jumps")
                wrapParams(ops - 'updateOnUncond, p.copy()) // TODO: implement this case
            }
            case o if (o.contains('useGem5)) => {
                println(f"Using gem5 indexing")
                wrapParams(ops - 'useGem5, p.copy(useGem5=true))
            }
            case o if (o.contains('useXS)) => {
                println(f"Using XS params")
                val xst: Seq[Tuple2[Int, Int]]  = Seq(( 2048,   7),
                                                      ( 2048,   7),
                                                      ( 4096,   8),
                                                      ( 4096,   8),
                                                      ( 2048,   9),
                                                      ( 2048,   9))
                wrapParams(ops - 'useXS, p.copy(useGem5=false, t=xst, SuperScalar=true, minHist=2, maxHist=64))
            }
            case o if (o.contains('useStatisticalCorrector)) => {
                println(f"Using statistical corrector")
                wrapParams(ops - 'useStatisticalCorrector, p.copy(useStatisticalCorrector=true))
            }
            case _ => 
                wrapParams(Map(), p)
        }
}

// object TageTest extends PredictorUtils{
//     def main(args: Array[String]): Unit = {
//         val bpd = new Tage
//         var hist: Long = 0
//         for (i <- 0 until 1000) {
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
