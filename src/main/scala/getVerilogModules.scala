package scalaTage

import scala.math._
import scala.util._
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Using
import scala.util.Try
import scala._
import scala.collection.mutable
import java.io._

class VerilogModuleExtractor() extends FileIOUtils {
    //                              module name
    val modulePattern = "module ([\\w]+)\\(".r.unanchored
    //                          type          name
    val subMoudlePattern = "([\\w]+) ([\\w]+) \\( ".r.unanchored
    val endMoudleIOPattern = "\\);".r.unanchored
    val endMoudlePattern = "endmodule".r.unanchored

    //                           (submoudle type, submoudle name)
    type SubMoudleRecord = Tuple2[String, String]

    //                        (content,          submodules)
    type ModuleRecord = Tuple2[List[String], List[SubMoudleRecord]]
    //                       name
    type ModuleMap = Map[String, ModuleRecord]

    def getLines(s: scala.io.BufferedSource): Iterator[String] = s.getLines()
    
    def makeRecord(s: Iterator[String]): ModuleMap = {
        val m: ModuleMap = Map()
        // called before we see the first line of a module
        def processModule(firstLine: String, it: Iterator[String]): ModuleRecord = {
            val content: List[String] = List(firstLine)
            val submodules: List[SubMoudleRecord] = List()
            def iter(cont: List[String], subm: List[SubMoudleRecord]): ModuleRecord = 
                it.next() match {
                    case l: String => l match {
                        case endMoudlePattern() => (cont ::: List(l), subm)
                        case subMoudlePattern(ty, name) => iter(cont ::: List(l), subm ::: List((ty, name)))
                        case _ => iter(cont ::: List(l), subm)
                    }
                    case _ => {println("Should not reach here"); (cont, subm) }
                }
            iter(content, submodules)
        }
        def traverse(m: ModuleMap, it: Iterator[String]): ModuleMap = 
            if (it.hasNext) {
                it.next() match {
                    case l: String => {
                        // println(f"traversing $l")
                        l match {
                            case modulePattern(name) => {
                                println(f"get Module of name $name")
                                traverse(m ++ Map(name -> processModule(l, it)), it)
                            }
                            case _ => {
                                println(f"line $l is not a module definition")
                                traverse(m, it)
                            }
                        }
                    }
                    case _ => traverse(m, it)
                }
            }
            else m
        
        traverse(m, s)
    }

    def makeRecordFromFile(file: String): ModuleMap = {
        readFile[ModuleMap](file, s => makeRecord(getLines(s))).get
    }

    def writeModuleToFile(name: String, record: ModuleRecord, dir: String = "/home/glr/scalaTage/verilog/") = {
        val path = dir+name+".v"
        println(f"Writing module $name to $path")
        writeToFile(path, w => {
            record._1.foreach(l => w.write(f"$l\n"))
        })
    }

    // get moudle definition of specified name
    def getModule(name: String, m: ModuleMap): ModuleRecord = {
        m(name)
    }

    def showModuleRecord(r: ModuleRecord) = {
        val (content, submodules) = r
        submodules.foreach {
            case (t, n) => println(f"submoudle type: $t, submodule name: $n")
        }
        println("\nprinting module contents...")
        content.foreach(println(_))
    }
    
    // We first get records of all the modules and its submodule record
    // Then we choose a module as the root node to traverse its submodule
    def processFromModule(name: String, map: ModuleMap): Unit = {
        val r = map(name)
        writeModuleToFile(name, r)
        val submodules = r._2
        // DFS
        submodules.foreach { s => processFromModule(s._1, map - "name") }
    }
}

object VMETest {
    def main(args: Array[String]): Unit = {
        val vme = new VerilogModuleExtractor()
        val map = vme.makeRecordFromFile("/home/glr/XiangShan/build/XSSimTop.v")
        // vme.showModuleRecord(map("IFU"))
        vme.processFromModule("IFU", map)
        // println(vme.makeRecordFromFile("/home/glr/XiangShan/build/XSSimTop.v"))
    }
}