package uk.co.danielrendall.adventofcode.y2021

import uk.co.danielrendall.adventofcode.utils.LazyListUtils.*
import uk.co.danielrendall.adventofcode.utils.OrderingUtils.*
import uk.co.danielrendall.adventofcode.utils.StreamUtils.*
import uk.co.danielrendall.adventofcode.utils.StringUtils.*

object Day17 {

  val testData: String = "target area: x=20..30, y=-10..-5"

  val data: String = this.getClass.getResourceAsStream("/2021/day17.txt").lines.filterNot(_.isEmpty).head

  @main def d17p1() =
    def solve(string: String): Unit =
      val target = Target(string)
      println(target)

      val allOptions = getAllOptions(target)

      // Maximum height = maximum y velocity
      val maxInitialY = allOptions.groupBy(_._2).maxBy(_._1)._2

      // Steepest angle
      val bestTrajectory = maxInitialY.groupBy(_._1).minBy(_._1)._2.head


      val highestPoint = getYTrajectory(YTrajectory(bestTrajectory._2), target.yMin).toList.maxBy(_.pos).pos
      println(s"Best trajectory: (${bestTrajectory._1}, ${bestTrajectory._2}) - highest point = $highestPoint")

    solve(testData)
    solve(data)

  @main def d17p2() =
    def solve(string: String): Unit =
      val target = Target(string)
      println(target)

      // We need distinct here, otherwise we multiply count trajectories that give more than one hit in the target
      val allOptions = getAllOptions(target).sorted.distinct
      println("Number of options = " + allOptions.size)

    solve(testData)
    solve(data)


  def getAllOptions(target: Target): Seq[(Int, Int)] =
    // X trajectories; all initial X velocities the result in hits in the target
    val xTrajectories: Seq[XTrajectory] = (1 to target.xMax).flatMap { initialX =>
      getXTrajectory(XTrajectory(initialX)).filter(_.isInBounds(target)).toList
    }

    // Y trajectories are harder; we need lower bound and upper bound of velocity
    // We will assume the target is always below the launch point
    // The fastest downward trajectory we could have would be aiming for yMin i.e. hit the lowest point of the target
    // at step 1 (thereafter overshooting)
    // To find the fastest _upward_ trajectory we need to bother with, note that the trajectory will be symmetric
    // i.e. after some steps the probe will be back to y=0 and heading downwards with some speed. If it hits the
    // lowest point of the target, its speed at this point is yMin, in which case its speed at the previous step was
    // (yMin + 1), which is equal in magnitude to the launch velocity
    // So a spread of -|yMin| to |yMin| will cover all possible velocities that could conceivably hit the target;
    // anything outside this range is guaranteed to overshoot.

    val absYMin = Math.abs(target.yMin)

    val yTrajectories: Seq[YTrajectory] = (-absYMin to absYMin).flatMap { initialY =>
      getYTrajectory(YTrajectory(initialY), target.yMin).filter(_.isInBounds(target)).toList
    }

    val allPossibleNumbersOfSteps = xTrajectories.map(_.step).toSet ++ yTrajectories.map(_.step).toSet
    val (minSteps, maxSteps) = allPossibleNumbersOfSteps.minMax.get

    // So no trajectory can possibly be longer than maxSteps
    // We need to fiddle the X trajectories; if any trajectory contains a final velocity of 0, then we need to extend
    // it to last for the maximum number of steps (because it will remain in the target area for ever and we can wait
    // for the Y trajectory to catch up)

    val fixedXTrajectories = xTrajectories.flatMap { t =>
      if (t.currentVel == 0)
        (t.step to maxSteps).map(s => t.copy(step = s))
      else Seq(t)
    }

    // So for any x and y trajectories that take the same number of steps, we get the probe in the target
    val xGrouped: Map[Int, Seq[XTrajectory]] = fixedXTrajectories.groupBy(_.step)
    val yGrouped: Map[Int, Seq[YTrajectory]] = yTrajectories.groupBy(_.step)
    (minSteps to maxSteps).flatMap { s =>
      (for {
        xTrajectoryOptions <- xGrouped.get(s)
        yTrajectoryOptions <- yGrouped.get(s)
      } yield
        for {
          x <- xTrajectoryOptions
          y <- yTrajectoryOptions
        } yield (x, y)
      ).getOrElse(Seq.empty).map { case (xTraj, yTraj) => (xTraj.initialVel, yTraj.initialVel) }
    }


  def getXTrajectory(initial: XTrajectory): LazyList[XTrajectory] =
    LazyList.unfold(initial) { current =>
      current.next.map(n => (n, n))
    }

  def getYTrajectory(initial: YTrajectory, yMin: Int): LazyList[YTrajectory] =
    LazyList.unfold(initial) { current =>
      val next = current.next
      if (next.pos >= yMin) {
        Some(next, next)
      } else {
        None
      }
    }

  case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

  case class XTrajectory(initialVel: Int, currentVel: Int, pos: Int, step: Int) {
    def next: Option[XTrajectory] = if (currentVel > 0) Some(XTrajectory(initialVel, currentVel - 1, pos + currentVel, step + 1)) else None
    def isInBounds(target: Target): Boolean = pos >= target.xMin && pos <= target.xMax
  }

  object XTrajectory {
    def apply(initialVelocity: Int): XTrajectory = XTrajectory(initialVelocity, initialVelocity, 0, 0)
  }

  case class YTrajectory(initialVel: Int, currentVel: Int, pos: Int, step: Int) {
    def next: YTrajectory = YTrajectory(initialVel, currentVel - 1, pos + currentVel, step + 1)
    def isInBounds(target: Target): Boolean = pos >= target.yMin && pos <= target.yMax
    def hasFallenOutOfBounds(target: Target): Boolean = pos < target.yMin
  }

  object YTrajectory {
    def apply(initialVelocity: Int): YTrajectory = YTrajectory(initialVelocity, initialVelocity, 0, 0)
  }

  object Target {

    private val TargetRegex = "target area: x=(-?[0-9]+)\\.\\.(-?[0-9]+), y=(-?[0-9]+)\\.\\.(-?[0-9]+)".r("x1", "x2", "y1", "y2")

    def apply(desc: String): Target =
      desc match {
        case TargetRegex(x1, x2, y1, y2) =>
          val (xMin, xMax) = minMax(x1.toInt, x2.toInt)
          val (yMin, yMax) = minMax(y1.toInt, y2.toInt)
          Target(xMin, xMax, yMin, yMax)
      }


  }
}
