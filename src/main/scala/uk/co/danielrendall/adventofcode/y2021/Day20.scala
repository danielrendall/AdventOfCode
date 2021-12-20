package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.ArrayUtils.*
import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day20 {

  val testData: LazyList[String] =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
      |#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
      |.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
      |.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
      |.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
      |...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
      |..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###""".stripMargin.splitAndTrimToList

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day20.txt").lines

  @main def d20p1() =
    def solve(seq: Seq[String]): Unit =
      afterGridN(seq, 2)

    solve(testData)
    solve(data)

  @main def d20p2() =
    def solve(seq: Seq[String]): Unit =
      afterGridN(seq, 50)

    solve(testData)
    solve(data)

  def afterGridN(seq: Seq[String], n: Int): Unit =
    val (line, grid) = parseSeq(seq)
    assert(line.length == 512)
    val lookup = buildLookup(line)
    val initial = grid

    val enhanced: LazyList[Array2D[Char]] = LazyList.unfold(initial) { g =>
      val expandedGrid = g.expand
      val updated = expandedGrid.map { loc =>
        lookup(Seq(loc.upLeft, loc.up, loc.upRight, loc.left, loc, loc.right, loc.downLeft, loc.down, loc.downRight).map(expandedGrid.get).mkString)
      }
      val newBorder = lookup(s"${g.borderValue}" * 9)
      val withNewBorder = updated.withNewBorder(newBorder)

      Some(withNewBorder, withNewBorder)
    }

    val allGrids: LazyList[Array2D[Char]] = initial #:: enhanced

    val finalGrid = allGrids.drop(n).head
    println("Width: " + finalGrid.width + " height: " + finalGrid.height)
    println(finalGrid.count(_ == '#'))



  def buildLookup(str: String): Map[String, Char] = {
    (0 to 511).map(i => Integer.toString(i, 2).reverse.padTo(9, '0').reverse.map {
      case '0' => '.'
      case '1' => '#'
    }).zipWithIndex.map { case (s, i) =>
//      println(i + ": " + s + " -> " + str.charAt(i))
      s -> str.charAt(i)
    }.toMap
  }



  def parseSeq(seq: Seq[String]): (String, Array2D[Char])  =
    def split(remaining: List[String], halfway: Boolean, firstBit: List[String], secondBit: List[String]): (String, Array2D[Char]) =
      remaining match {
        case head :: rest =>
          if (head.isEmpty && !halfway) {
            split(rest, true, firstBit, secondBit)
          } else if (!halfway) {
            split(rest, halfway, head :: firstBit, secondBit)
          } else {
            split(rest, halfway, firstBit, head :: secondBit)
          }
        case _ =>
          (firstBit.reverse.mkString, buildBorderedArray[Char](secondBit.reverse, identity, '.'))
      }
    split(seq.toList, false, List.empty, List.empty)


}
