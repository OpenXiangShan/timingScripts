import scala.collection.mutable

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
            case (1 << ctrBits) - 1 if (taken) => 7
            case 0 if (!taken) => 0
            _ => if (taken) {oldCtr + 1} else {oldCtr - 1}
        }
    }
    def log2(x: Int): Int = (log(x) / log(2)).toInt
    def getMask(len: Int) = (1 << len) - 1
    def getBit(x: Long, pos: Int): Int = x & (1 << pos)
    def getBits(x: Long, from: Int, to: Int): Int = (x & (((1 << (to - from + 1)) - 1) << from)) >> from
    def divUp(x: Int, y: Int): Int = (x / y) + if (x % y) 1 else 0

    def boolArrayToInt(arr: Array[Boolean]): Int = arr.zipWithIndex.map(case (b, i) => b.toInt << i).reduce(_|_)
    def intToBoolArray(x: Int, len: Int): Array[Boolean] = new Array((0 until len).map(i => (x >> i) % 2 != 0))
    def PriorityEncoder(arr: Array[Boolean]): Int = {res = len(arr); arr.zipWithIndex.reverse.foreach(case(b,i) => if (b) res = i)}
    def PriorityEncoder(x: Int, len: Int): Int = PriorityEncoder(intToBoolArray(x, len))
}

trait GTimer {
    var cycle: Long = 0
    def step(x: Long) = cycle += x
    def isCycle(x: Long): Boolean = cycle == x
}

class GlobalHistory(val maxlen: Int) {
    val hist: Array[Boolean] = new Array(Seq.fill(maxlen, false))
    var ptr: Int = 0
    def getHistPtr = this.ptr
    def getHist(len: Int, ptr: Int = this.ptr): Array[Boolean] = {
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

    val banks: Array[Array[Entry]] = new Array(Seq.fill(TageBanks, new Array(Seq.fill(nRows, new Entry))))

    def ctrUpdate(oldCtr: Int, taken: Boolean): Int = satUpdate(taken, oldCtr, 3)

    def getTag(pc: Long) = pc & tagMask
    def getBank(pc: Long) = (pc >> 1) & bankMask
    def getUnhashedIdx(pc: Long) = pc >> (1 + log2(TageBanks))
    def foldHist(hist: Long, len: Int) = range(divUp(histLen, len)).map(i => (hist >> (i*len)) & getMask(len)).reduce(_^_)
    def getIdx(unhashed: Long, hist: Long) = (unhashed ^ foldHist(hist, log2(nRows))) & rowMask
    def getTag(unhashed: Long, hist: Long) = (unhashed ^ foldHist(hist, tagLen)) & tagMask

    // Superscalar lookup
    def lookUp(pc: Long, hist: Long, mask: Array[Boolean]): TableResp = {
        val baseBank = getBank(pc)
        val entries = range(TageBanks).map(b => banks((baseBank+b)&bankMask)(getIdx(getUnhashedIdx(pc + 2 * b), hist)))
        val tags = range(TageBanks).map(b => getTag(getUnhashedIdx(pc + 2 * b)))
        entries.map(e => new TableResp(e.ctr, e.u, range(TageBanks).map(b => tags(b) == e.tag && e.valid && mask(b)))
    }

    // Per-bank lookup
    def lookUp(pc: Long, hist: Long): TableResp = {
        lookUp(pc, hist, new Array(range(TageBanks).map(_ == bank)))(bank)
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
            banks(bank)(idx).ctr = if (alloc) {if (taken) 4 else 3} else ctrUpdate(oldCtr, taken)
        }

        if (uValid) banks(bank)(idx).u = u
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
    val table = new Array(Seq.fill(nEntries, 0))
    val ctrBits = 2
    val idxMask = getMask(log2(nEntries))
    def getIdx(pc: Long): Int = pc & idxMask
    def ctrUpdate(old: Int, taken: Boolean): Int = satUpdate(old, taken, ctrBits)
    def lookUp(pc: Long) = table(getIdx(pc))
    def update(pc: Long, taken: Boolean) = table.update(table(getIdx(pc)), ctrUpdate(table(getIdx(pc)), taken))
}

class Tage extends TageParams with Utils{
    val speculativeUpdate = false
    val instantUpdate = true

    var brCount = 0

    val bim = new Bim(BimEntries)
    val tables = TableInfo.map(case(nRows, histLen, tagLen) => new TageTable(nRows, histLen, tagLen, UBitPeriod))
    val ghist = new GlobalHistory(maxlen = 128)

    

    class TageMeta(val pc: Long, val histPtr: Int, val pvdr: Int, val pvdrValid: Boolean,
        val altDiff: Boolean, val pvdrU: Int, val pvdrCtr: Int, val alloc: Int,
        val allocValid: Boolean) {}

    val predictMetas = new mutable.Queue[TageMeta]

    def predict(pc: Long): Boolean = {
        val hist = ghist.getHist(t.histLen)
        val tableResps = tables.map(t => t.lookUp(pc, hist))
        val bimResp = bim.lookUp(pc) >= 2
        var altPred = bimResp
        var provided = false
        var provider = 0
        var res = bimResp
        var finalAltPred = bimResp
        for (i <- 0 until TageNTables) {
            val hit = tableResps(i).hit
            val ctr = tableResps(i).ctr
            res = if (ctr == 3 || ctr == 4) altPred else ctr >= 4
            finalAltPred = if (hit) altPred else finalAltPred
            provided ||= hit
            provider = if (hit) i else provider
            altPred = if (hit) ctr >= 4 else altPred
        }
        val allocatableArray = tableResps.map(r => !r.hit && r.u == 0 && r > provider)
        val allocatable = boolArrayToInt(allocatableArray)
        val allocMask   = scala.util.Random.nextInt((1 << TageNTables))
        val maskedEntry = PriorityEncoder(allocatable & allocMask, TageNTables)
        val firstEntry  = PriorityEncoder(allocatable, TageNTables)
        val allocEntry  = if (allocatableArray(maskedEntry), maskedEntry, firstEntry)
        predictMetas.enqueue(new TageMeta(pc, ghist.getHistPtr, provider, 
            provided, res != finalAltPred, tableResps(provider).u,
            tableResps(provider).ctr, allocEntry, allocatable != 0))
        brCount += 1
        res
    }

    def update(pc: Long, taken: Boolean, pred: Boolean) {
        val meta = predictMetas.dequeue()
        if (pc != meta.pc) println("update pc does not correspond with expected pc\n")
        
        bim.update(pc, taken)

        val misPred = taken != pred
        val hist = getHist(table.histLen, meta.histPtr)
        if (meta.pvdrValid) {
            tables(pvdr).pvdrUpdate(pc, hist, taken, meta.pvdrCtr, 
                if(meta.altDiff) satUpdate(!misPred, meta.pvdrU, 2) else meta.pvdrU)
        }
        if (misPred) {
            if (meta.allocValid) tables(alloc).allocUpdate(pc, hist, taken)
            else (0 until TageNTables).foreach(i => if (i > meta.pvdr) tables(i).decrementU(pc, hist))
        }
    }
}
