package vme

import scala.math._
import scala.util._
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Using
import scala.util.Try
import scala._
import scala.collection.mutable
import java.io._
import scala.language.postfixOps

trait FileIOUtils {
    // provides an interface to handle open and close when writing files
    def writeToFile(f: String, action: (java.io.PrintWriter) => Unit) = {
        val writer = new PrintWriter(new File(f))
        action(writer)
        writer.close()
    }

    def readFile[T](f: String, action: (scala.io.BufferedSource) => T): Try[T] = {
        Using(Source.fromFile(f)) { s => action(s) }
    }
}

class VerilogModuleExtractor() extends FileIOUtils {
    //                            name
    val modulePattern = "module ([\\w]+)\\(".r.unanchored
    //                       type      name
    val subMoudlePattern = "([\\w]+) ([\\w]+) \\((?: //.*)*\\Z".r.unanchored
    val endMoudleIOPattern = "\\);".r.unanchored
    val endMoudlePattern = "endmodule".r.unanchored

    //                           (submoudle type, submoudle name)
    type SubMoudleRecord = Tuple2[String, String]

    //                        (content,          submodules)
    type ModuleRecord = Tuple2[List[String], List[SubMoudleRecord]]
    //                   name
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
                        case endMoudlePattern() => (l :: cont, subm)
                        case subMoudlePattern(ty, name) => {
                            // println(s"submoudle $ty $name")
                            iter(l :: cont, (ty, name) :: subm)
                        }
                        case _ => iter(l :: cont, subm)
                    }
                    case _ => {println("Should not reach here"); (cont, subm) }
                }
            val temp = iter(content, submodules)
            (temp._1.reverse, temp._2)
        }
        def traverse(m: ModuleMap, it: Iterator[String]): ModuleMap = 
            if (it.hasNext) {
                it.next() match {
                    case l: String => {
                        // println(f"traversing $l")
                        l match {
                            case modulePattern(name) => {
                                // println(f"get Module of name $name")
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

    def writeModuleToFile(name: String, record: ModuleRecord, dir: String) = {
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
    def processFromModule(name: String, map: ModuleMap, outPath: String, doneSet: Set[String] = Set()): Unit = {
        def printSRAMs(sub: List[SubMoudleRecord]) = {
            sub map { t => t match {
                case (ty, subn) if (ty contains "SRAM") => println(s"top module $name, sub module type $ty, name $subn")
                case _ =>
            }}
        }
        
        val r = map(name)
        new File(outPath).mkdirs() // ensure the path exists
        writeModuleToFile(name, r, outPath)
        val submodules = r._2
        // printSRAMs(submodules)
        // DFS
        val subTypesSet = submodules map (m => m._1) toSet
        val nowMap = map - "name"
        val nowSet = doneSet ++ subTypesSet
        subTypesSet.foreach { s  => if (!doneSet.contains(s)) processFromModule(s, nowMap, outPath, nowSet) }
    }

    def getDate: String = {
        val d = java.time.LocalDate.now
        d.toString().toCharArray().filterNot(_ == '-').mkString
    }

    def makePath(topModule: String, outDir: String , user: String = "glr"): String = {
        (if (outDir.last == '/')
            outDir
        else
            outDir+"/") + getDate + "-" + user + "-" + topModule + "/"
    }

    def extract(src: String, topModule: String, outDir: String, user: String, mapp: Option[ModuleMap]): Unit = {
        val useMap = mapp.getOrElse(makeRecordFromFile(src))
        val path = makePath(topModule, outDir, user)
        processFromModule(topModule, useMap, path)
    }

    def extract(src: String, topModules: List[String], outDir: String, user: String): Unit = {
        // avoid repeat
        val mapp = makeRecordFromFile(src)
        topModules.foreach(n => extract(src, n, outDir, user, Some(mapp)))
    }
}

object VMETest {
    def main(args: Array[String]): Unit = {
        // the verilog file generated by chisel, all in one file
        val sourceFile = "/home/glr/xs_alt/XiangShan/build/XSSimTop.v"
        // the name of the top modules you want to extract verilog of, could be String list or String
        val topModules = List("MicroBTB", "Tage", "BIM", "LoopPredictor")
        // the place you want to store your extracted verilog
        val outTopDir = "/home/glr/xs_verilog/"
        // your name
        val usr = "glr"

        val vme = new VerilogModuleExtractor()
        vme.extract(sourceFile, topModules, outTopDir, usr)
    }
}
