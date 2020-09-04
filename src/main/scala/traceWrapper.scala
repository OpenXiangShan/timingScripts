package scalaTage

import scala.io.Source
import scala.util.matching.Regex

case class CFIInfo( 
    // val cycle: Int,
    val isBr: Boolean,
    val pc: Long,
    val taken: Boolean,
    val misPred: Boolean
){
    override def toString: String = f"isBr($isBr%b) pc($pc%x) taken($taken%b) mispred($misPred%b)\n"
}

class TraceWrapper() {
    val cfiUpdatePattern = "cfi_update".r
    val cfiInfoExtractPattern = raw"isBr\(([0-1])\) pc\(([0-9|a-f]{10})\) taken\(([0-1])\) mispred\(([0-1])\)".r.unanchored

    def toBoolean(s: String): Boolean = 
        s match {
            case "1" => true
            case "0" => false
            case _ => {println("toBoolean error"); false}
        }

    def reMatch(str: String, p: Regex) = p findAllMatchIn str

    def getLines(file: String): Array[String] = Source.fromFile(file).getLines().toArray
    
    def checkLine(l: String): String = if ((cfiUpdatePattern findAllIn l).length != 0) l else " "

    def getCfiUpdates(file: String): Array[String] = (getLines(file).map { l => checkLine(l) } filter (_ != " ")) toArray

    def getCFIInfo(u: String): CFIInfo = {
        u match {
            case cfiInfoExtractPattern(isBr, pc, taken, misPred) =>
                CFIInfo(toBoolean(isBr), java.lang.Long.parseLong(pc.trim(), 16), toBoolean(taken), toBoolean(misPred))
            case _ => {println(" not matched" + u); CFIInfo(false, 0, false, false)}
        }
    }

    def getCFIInfos(updates: Array[String]): Array[CFIInfo] = updates.map(u => getCFIInfo(u))

    def getCFIInfosFromFile(file: String): Array[CFIInfo] = getCFIInfos(getCfiUpdates(file))

}

object WrapperTest{
    def main(args: Array[String]): Unit = {
        val tw = new TraceWrapper
        val file = "/home/glr/nexus-am/tests/cputest/build/dummy.log"
        tw.getCFIInfosFromFile(file).foreach(println)
    }
}

