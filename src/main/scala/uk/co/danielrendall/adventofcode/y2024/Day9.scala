package uk.co.danielrendall.adventofcode.y2024

import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

import scala.annotation.tailrec
import scala.collection.mutable

object Day9 {

  val testData: LazyList[String] =
    """2333133121414131402""".stripMargin.splitAndTrimToList

  val numbers: LazyList[String] = this.getClass.getResourceAsStream("/2024/day9.txt").lines

  def applyDiskMap(disk: Array[Int],
                   remainingMap: List[Int],
                   pointer: Int,
                   fileId: Int,
                   isFile: Boolean): Int = remainingMap match
    case head :: rest =>
      if (isFile) {
        (0 until head).foreach(offset => disk(pointer + offset) = fileId)
        applyDiskMap(disk, rest, pointer + head, fileId + 1, false)
      } else {
        // Should be -1 already, of course...
        (0 until head).foreach(offset => disk(pointer + offset) = -1)
        applyDiskMap(disk, rest, pointer + head, fileId, true)
      }
    case _ =>
      pointer

  @tailrec
  def compact(disk: Array[Int], emptySpacePointer: Int, fileToCopyPointer: Int): Unit =
    if (fileToCopyPointer < emptySpacePointer) () else {
      if (disk(emptySpacePointer) != -1) {
        compact(disk, emptySpacePointer + 1, fileToCopyPointer)
      } else if (disk(fileToCopyPointer) == -1) {
        compact(disk, emptySpacePointer, fileToCopyPointer - 1)
      } else {
        disk(emptySpacePointer) = disk(fileToCopyPointer)
        disk(fileToCopyPointer) = -1
        compact(disk, emptySpacePointer + 1, fileToCopyPointer - 1)
      }
    }

  def buildAndLinkDisk(remaining: List[Int],
                       last: DiskPart,
                       fileId: Int,
                       isFile: Boolean): Unit =
    remaining match
      case head :: rest =>
        if (isFile) {
          val file = new File(fileId, head)
          last.next = file
          file.previous = last
          buildAndLinkDisk(rest, file, fileId + 1, false)
        } else {
          val space = new Space(head)
          last.next = space
          space.previous = last
          buildAndLinkDisk(rest, space, fileId, true)
        }
      case _ => ()


  def compact(head: DiskPart, file: File): Unit =
    val size = file.size
    // Find the previous file now, before we start moving things...
    var previousFile: DiskPart = file.previous
    while ((previousFile != null) && (!previousFile.isInstanceOf[File] || (previousFile.asInstanceOf[File].id != (file.id - 1)))) {
      previousFile = previousFile.previous
    }
    if (previousFile == null) {
      // run out of files to move
      ()
    } else {
      var nextSpace: DiskPart = head
      while ((nextSpace != null) && (nextSpace != file) && (!nextSpace.isInstanceOf[Space] || (nextSpace.asInstanceOf[Space].size < file.size))) {
        nextSpace = nextSpace.next
      }
      if ((nextSpace == null) || (nextSpace eq file) || (!nextSpace.isInstanceOf[Space])) {
        // No point moving file to the right
        compact(head, previousFile.asInstanceOf[File])
      } else {
        val targetSpace = nextSpace.asInstanceOf[Space]

        val newSpace = new Space(file.size)
        file.previous.next = newSpace
        newSpace.previous = file.previous
        if (file.next != null) {
          file.next.previous = newSpace
          newSpace.next = file.next
        }
        file.next = null
        file.previous = null
        // file now detached, new space of same size now in its place
        val diff = targetSpace.size - file.size
        assert(diff >= 0, "Shouldn't have tried to fit file into space which is too small")
        targetSpace.previous.next = file
        file.previous = targetSpace.previous
        if (diff == 0) {
          targetSpace.next.previous = file
          file.next = targetSpace.next
        } else {
          val newSpace = new Space(diff)
          newSpace.previous = file
          file.next = newSpace
          targetSpace.next.previous = newSpace
          newSpace.next = targetSpace.next
        }
        targetSpace.previous = null
        targetSpace.next = null
        // targetSpace now detached

        compact(head, previousFile.asInstanceOf[File])
      }
    }

  def computeChecksum(head: Head): Long = {

    @tailrec
    def compute(part: DiskPart, pos: Int, accum: Long): Long =
      if (part == null) accum else {
        part match
          case head: Head =>
            compute(part.next, 0, 0L)
          case file: File =>
            val contribution = (pos until (pos + file.size)).map(_ * file.id.toLong).sum
            compute(part.next, pos + file.size, accum + contribution)
          case space: Space =>
            compute(space.next, pos + space.size, accum)
      }

    compute(head, 0, 0L)
  }

  @main def d9p1(): Unit = {
    def solve(list: LazyList[String]) =
      val map: List[Int] = list.head.map(_.toInt - '0').toList
      val size = map.sum
      val disk: Array[Int] = Array.fill(size)(-1)
      val finalPointer = applyDiskMap(disk, map, 0, 0, true)
      compact(disk, 0, finalPointer - 1)
      disk.to(LazyList).zipWithIndex.takeWhile(_._1 != -1).map { case (a, b) => a.toLong * b.toLong }.sum

    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  @main def d9p2(): Unit = {
    def solve(list: LazyList[String]) =
      val map: List[Int] = list.head.map(_.toInt - '0').toList
      val head = new Head
      buildAndLinkDisk(map, head, 0, true)
      var last: DiskPart = head
      while (last.next != null) {
        last = last.next
      }
      compact(head, last.asInstanceOf[File])
      computeChecksum(head)


    println("Test: " + solve(testData))
    println("Actual: " + solve(numbers))
  }

  sealed abstract class DiskPart {
    var previous: DiskPart = null
    var next: DiskPart = null
    def getDesc: String
    def toDiskMap: String = {
      val sb = new StringBuilder()
      var diskPart = this
      while (diskPart != null) {
        sb.append(diskPart.getDesc)
        diskPart = diskPart.next
      }
      sb.toString()
    }
  }

  class Head extends DiskPart {
    override def getDesc = ""

    override def toString: String = "HEAD"
  }

  class File(val id: Int, val size: Int) extends DiskPart {
    override def getDesc: String = s"$id" * size

    override def toString: String = s"F($id, $size)"
  }

  class Space(val size: Int) extends DiskPart {
    override def getDesc: String = "." * size

    override def toString: String = s"S($size)"
  }
}

