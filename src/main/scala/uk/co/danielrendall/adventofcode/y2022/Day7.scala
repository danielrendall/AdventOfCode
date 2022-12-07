package uk.co.danielrendall.adventofcode.y2022

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

object Day7 {


  val testData: LazyList[String] =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin.splitAndTrimToList

  val games: LazyList[String] = this.getClass.getResourceAsStream("/2022/day7.txt").lines.filterNot(_.isEmpty)

  @main def d7p1(): Unit =
    def solve(list: LazyList[String]) =
      val root = buildFileSystem(list)
      val dirs = root.find(_.totalSize < 100000)
      dirs.map(_.totalSize).sum


    println("Test: " + solve(testData))
    println("Actual: " + solve(games))

  @main def d7p2(): Unit =
    def solve(list: LazyList[String]) =
      val root = buildFileSystem(list)
      val diskSpace = 70000000L
      val required = 30000000L
      val used = root.totalSize
      val free: Long = diskSpace - used
      assert (free < required)
      val needToFind = required - free

      val dir = root.find(_.totalSize > needToFind).minBy(_.totalSize)
      dir.totalSize


    println("Test: " + solve(testData))
    println("Actual: " + solve(games))


  private def buildFileSystem(list: LazyList[String]): DirNode =
    val root: DirNode = new DirNode("", null)
    var current: DirNode = root
    list.groupStartingWith(_.startsWith("$")).foreach { s =>
      s.toList match
        case head :: rest =>
          if (head == "$ cd /") {
            current = root
          } else if (head == "$ ls") {
            rest.foreach { line =>
              val bits = line.split(' ')
              if (bits(0) == "dir") {
                current.addChild(bits(1))
              } else {
                current.addFile(bits(1), bits(0).toInt)
              }
            }
          } else if (head.startsWith("$ cd ")) {
            val where = head.substring(5)
            if (where == "..") {
              current = current.parent
            } else {
              current = current.child(where)
            }
          }
        case Nil =>
          throw new IllegalStateException("Shouldn't have had an empty list")
    }
    root


  class DirNode(val name: String, val parent: DirNode) {

    private val childDirs: mutable.Map[String, DirNode] = new mutable.HashMap()
    private val files: mutable.Map[String, Long] = new mutable.HashMap()

    private var sizeOpt: Option[Long] = Option.empty
    private var totalSizeOpt: Option[Long] = Option.empty

    private def reset(): Unit = {
      sizeOpt = Option.empty
      totalSizeOpt = Option.empty
    }

    def addChild(name: String): Unit = {
      reset()
      childDirs.put(name, new DirNode(name,this))
    }

    def addFile(name: String, size: Long): Unit = {
      reset()
      files.put(name, size)
    }

    def child(name: String): DirNode = childDirs(name)

    def size: Long = sizeOpt.getOrElse {
      val s = files.values.sum
      sizeOpt = Some(s)
      s
    }

    def totalSize: Long = totalSizeOpt.getOrElse {
      val s = size + childDirs.values.map(_.totalSize).sum
      totalSizeOpt = Some(s)
      s
    }

    def absPath: String = Option(parent) match
      case Some(value) => value.absPath + "/" + name
      case None => ""

    def list: List[String] = {
      val myPath = absPath
      files.toList.sortBy(_._1).map(f => myPath + "/" + f._1 + " " + f._2) ++ childDirs.toList.sortBy(_._1).flatMap(_._2.list)
    }

    def find(predicate: DirNode => Boolean): List[DirNode] =
      childDirs.values.flatMap(_.find(predicate)).toList ++ (if (predicate(this)) Seq(this) else Seq.empty)
  }

}
