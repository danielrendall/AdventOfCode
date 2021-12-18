package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day18 {

  val testData: LazyList[String] =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.splitAndTrimToList

  val explodeTestData: Seq[(String, String)] =
    Seq(
      ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
      ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
      ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
      ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
      ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    )

  val data: LazyList[String] = this.getClass.getResourceAsStream("/2021/day18.txt").lines.filterNot(_.isEmpty)

  @main def d18p1() =
    def solve(seq: Seq[String]): Unit =
      val result: ImmutableBranch = seq.map(ImmutableTree.apply).reduceLeft { case (b1, b2) => add(b1, b2)}
      println(result)
      println(result.magnitude)

    solve(testData)
    solve(data)

  @main def d18p2() =
    def solve(seq: Seq[String]): Unit =
      val trees: Seq[ImmutableBranch] = seq.map(ImmutableTree.apply)

      val result = (for {
        b1 <- trees
        b2 <- trees
        if (b1 != b2)
      } yield {
        val added = add(b1, b2)
        (added, added.magnitude)
      }).maxBy(_._2)
      println(result._1)
      println(result._2)

    solve(testData)
    solve(data)


  def add(left: ImmutableBranch, right: ImmutableBranch): ImmutableBranch =
    reduce(ImmutableBranch(left, right))

  def reduce(tree: ImmutableBranch): ImmutableBranch =
    val mutable = tree.toMutable

    val list = LazyList.unfold(mutable) { current =>
      explode(current) match {
        case Some(exploded) =>
          Some((exploded, exploded))
        case None =>
          split(current) match {
            case Some(split) =>
              Some((split, split))
            case None =>
              None
        }
      }
    }
    list.lastOption.map(_.toImmutable).getOrElse(tree)


  def explode(mutable: MutableBranch): Option[MutableBranch] = {
    mutable.firstExplodableOpt.map { explodable =>
        // We're assured this will always work...
        val leftValue = explodable.left.getValue
        val rightValue = explodable.right.getValue

        val leafToLeft = explodable.firstLeafToLeftOpt
        val leafToRight = explodable.firstLeafToRightOpt

        leafToLeft.foreach { leaf =>
          leaf.value = leaf.value + leftValue
        }

        leafToRight.foreach { leaf =>
          leaf.value = leaf.value + rightValue
        }

        explodable.parentOpt.foreach { parentDetails =>
          parentDetails.side match {
            case LeftSide => parentDetails.mutableBranch.left = new MutableLeaf(0, explodable.depth, parentDetails)
            case RightSide => parentDetails.mutableBranch.right = new MutableLeaf(0, explodable.depth, parentDetails)
          }
        }
        mutable
    }
  }

  def split(mutable: MutableBranch): Option[MutableBranch] = {
    mutable.firstSplittableOpt.map { splittable =>
      val (l, r) = splittable.value match {
        case n if (n % 2 == 0) => (n / 2, n / 2)
        case n if (n % 2 == 1) => ((n - 1) / 2, (n + 1) / 2)
      }

      val mb = new MutableBranch(null, null, splittable.depth, Some(splittable.parent))
      mb.left = new MutableLeaf(l, splittable.depth + 1, ParentDetails(mb, LeftSide))
      mb.right = new MutableLeaf(r, splittable.depth + 1, ParentDetails(mb, RightSide))
      splittable.parent.side match {
        case LeftSide => splittable.parent.mutableBranch.left = mb
        case RightSide => splittable.parent.mutableBranch.right = mb
      }
      mutable
    }
  }

  // The thing into which we build the initial tree
  sealed trait ImmutableTree {

    def toMutable(depth: Int, parentOpt: Option[ParentDetails]): MutableTree

    def magnitude: Int

  }

  object ImmutableTree {

    def apply(string: String): ImmutableBranch = apply(ListWrapper(string.to(LazyList))).asInstanceOf[ImmutableBranch]

    def apply(chars: ListWrapper): ImmutableTree = chars.read match {
      case '[' =>
        val left = apply(chars)
        chars.read match {
          case ',' =>
            val right = apply(chars)
            chars.read match {
              case ']' => ImmutableBranch(left, right)
              case _ => throw new IllegalArgumentException("Expected ]")
            }
          case _ => throw new IllegalArgumentException("Expected ,")
        }
      case n => ImmutableLeaf(s"$n".toInt)
    }

  }

  case class ImmutableLeaf(value: Int) extends ImmutableTree {
    override def toString: String = value.toString

    override def toMutable(depth: Int, parentOpt: Option[ParentDetails]): MutableTree =
      new MutableLeaf(value, depth, parentOpt.getOrElse(throw new Exception("Leaf must have a parent")))

    override def magnitude: Int = value
  }

  case class ImmutableBranch(left: ImmutableTree, right: ImmutableTree) extends ImmutableTree {
    override def toString: String = s"[$left,$right]"

    def toMutable: MutableBranch = toMutable(0, None).asInstanceOf[MutableBranch]

    def toMutable(depth: Int, parentOpt: Option[ParentDetails]): MutableTree = {
      val mutableBranch = new MutableBranch(null, null, depth, parentOpt)
      val leftMutable = left.toMutable(depth + 1, Some(ParentDetails(mutableBranch, LeftSide)))
      val rightMutable = right.toMutable(depth + 1, Some(ParentDetails(mutableBranch, RightSide)))
      mutableBranch.left = leftMutable
      mutableBranch.right = rightMutable
      mutableBranch
    }

    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  // Nasty mutability!
  sealed trait Side
  case object LeftSide extends Side {
    override def toString: String = "L"
  }
  case object RightSide extends Side {
    override def toString: String = "R"
  }

  sealed trait TraversalState {
    def currentNode: MutableTree
  }
  case class Down(currentNode: MutableTree) extends TraversalState
  case class UpFromRight(currentNode: MutableBranch) extends TraversalState
  case class UpFromLeft(currentNode: MutableBranch) extends TraversalState


  sealed trait MutableTree {
    def depth: Int
    def parentOpt: Option[ParentDetails]

    def getValue: Int = throw new UnsupportedOperationException("Can't get value")

    def toImmutable: ImmutableTree

    def toSimpleString: String = toImmutable.toString

    def firstExplodableOpt: Option[MutableBranch]

    def firstSplittableOpt: Option[MutableLeaf]

    def treeNodesToLeft: LazyList[MutableTree] =
      val startBranchOpt: Option[MutableBranch] = this match {
        case ml:MutableLeaf =>
          // If we're a leaf, we need to find the first branch on the path to the root where we're coming up from the right hand side
          ml.parentDetailsPathToRoot.find(_.side == RightSide).map(_.mutableBranch)

        case mb:MutableBranch => Some(mb)
      }
      startBranchOpt.map { startBranch =>
        // We pretend we've hit this start node from the right...
        LazyList.unfold[MutableTree, TraversalState](UpFromRight(startBranch)) {
          case Down(tree) =>
            Some(tree match {
              case ml: MutableLeaf =>
                val nextBranch = ml.parent.mutableBranch
                ml.parent.side match {
                case LeftSide => (nextBranch, UpFromLeft(nextBranch))
                case RightSide => (nextBranch, UpFromRight(nextBranch))
              }
              case mb: MutableBranch =>
                (mb.right, Down(mb.right))
            })
          case UpFromRight(branch) =>
            Some((branch.left, Down(branch.left)))

          case UpFromLeft(branch) =>
            branch.parentOpt.map { parent =>
              val nextBranch = parent.mutableBranch
              parent.side match {
                case LeftSide => (nextBranch, UpFromLeft(nextBranch))
                case RightSide => (nextBranch, UpFromRight(nextBranch))
              }
            }
        }
      }.getOrElse(LazyList.empty)



    def treeNodesToRight: LazyList[MutableTree] =
      val startBranchOpt: Option[MutableBranch] = this match {
        case ml:MutableLeaf =>
          // If we're a leaf, we need to find the first branch on the path to the root where we're coming up from the left hand side
          ml.parentDetailsPathToRoot.find(_.side == LeftSide).map(_.mutableBranch)

        case mb:MutableBranch => Some(mb)
      }
      startBranchOpt.map { startBranch =>
        // We pretend we've hit this start node from the left...
        LazyList.unfold[MutableTree, TraversalState](UpFromLeft(startBranch)) {
          case Down(tree) =>
            Some(tree match {
              case ml: MutableLeaf =>
                val nextBranch = ml.parent.mutableBranch
                ml.parent.side match {
                  case LeftSide => (nextBranch, UpFromLeft(nextBranch))
                  case RightSide => (nextBranch, UpFromRight(nextBranch))
                }
              case mb: MutableBranch =>
                (mb.left, Down(mb.left))
            })
          case UpFromRight(branch) =>
            branch.parentOpt.map { parent =>
              val nextBranch = parent.mutableBranch
              parent.side match {
                case LeftSide => (nextBranch, UpFromLeft(nextBranch))
                case RightSide => (nextBranch, UpFromRight(nextBranch))
              }
            }
          case UpFromLeft(branch) =>
            Some((branch.right, Down(branch.right)))

        }
      }.getOrElse(LazyList.empty)


    def parentDetailsPathToRoot: LazyList[ParentDetails]

  }

  class MutableLeaf(var value: Int,
                    var depth: Int,
                    var parent: ParentDetails) extends MutableTree {
    override def parentOpt: Option[ParentDetails] = Some(parent)

    override def toString: String = s"${parent.side}$depth:$value"

    def toImmutable: ImmutableLeaf = ImmutableLeaf(value)

    override def firstExplodableOpt: Option[MutableBranch] = None

    override def firstSplittableOpt: Option[MutableLeaf] = if (value > 9) Some(this) else None

    override def parentDetailsPathToRoot: LazyList[ParentDetails] = parent #:: parent.mutableBranch.parentDetailsPathToRoot

    override def getValue: Int = value
  }

  class MutableBranch(var left: MutableTree,
                      var right: MutableTree,
                      var depth: Int,
                      var parentOpt: Option[ParentDetails]) extends MutableTree {
    override def toString: String = s"[${parentOpt.map(_.side.toString).getOrElse("")}$depth:$left,$right]"

    def toImmutable: ImmutableBranch = ImmutableBranch(left.toImmutable, right.toImmutable)

    def isExplodable: Boolean = depth == 4

    override def firstExplodableOpt: Option[MutableBranch] =
      if (isExplodable) Some(this)
      else left.firstExplodableOpt.orElse(right.firstExplodableOpt)

    override def firstSplittableOpt: Option[MutableLeaf] =
      left.firstSplittableOpt.orElse(right.firstSplittableOpt)

    override def parentDetailsPathToRoot: LazyList[ParentDetails] = parentOpt match {
      case Some(parent) => parent #:: parent.mutableBranch.parentDetailsPathToRoot
      case _ => LazyList.empty
    }

    def firstLeafToLeftOpt: Option[MutableLeaf] = treeNodesToLeft.collect {
      case mutableLeaf: MutableLeaf => mutableLeaf
    }.drop(1).headOption

    def firstLeafToRightOpt: Option[MutableLeaf] = treeNodesToRight.collect {
      case mutableLeaf: MutableLeaf => mutableLeaf
    }.drop(1).headOption
  }

  // A MutableTree's parent, and which of the two branches it is for that parent
  case class ParentDetails(mutableBranch: MutableBranch, side: Side)



  // Mutable
  class ListWrapper(initial: LazyList[Char]) {
    var current: LazyList[Char] = initial

    def read: Char =
      current.headOption match {
        case Some(next) =>
          current = current.tail
          next
        case _ => throw new Exception("Ran out of chars trying to read")
      }
  }

  object ListWrapper {
    def apply(initial: LazyList[Char]) = new ListWrapper(initial)
  }

}
