package uk.co.danielrendall.adventofcode.utils

sealed trait Move {
  def distance: Int

  def inverse: Move
}

object Move {

  sealed trait MoveCompanion {
    def unit: Move
    def apply(distance: Int): Move
  }

  case object NoMove extends Move {
    override val distance: Int = 0
    override val inverse: Move = NoMove
  }

  case class Up(distance: Int) extends Move {
    override def inverse: Move = downBy(distance)
  }

  object Up extends MoveCompanion  {
    override val unit: Move = Up(1)
  }

  val upBy: Int => Move = buildMove(Up)

  val up: Move = upBy(1)

  case class UpLeft(distance: Int) extends Move {
    override def inverse: Move = downRightBy(distance)
  }

  object UpLeft extends MoveCompanion  {
    override val unit: Move = UpLeft(1)
  }

  val upLeftBy: Int => Move = buildMove(UpLeft)

  val upLeft: Move = upLeftBy(1)

  case class Left(distance: Int) extends Move {
    override def inverse: Move = rightBy(distance)
  }

  object Left extends MoveCompanion {
    override val unit: Move = Left(1)
  }

  val leftBy: Int => Move = buildMove(Left)

  val left: Move = leftBy(1)

  case class DownLeft(distance: Int) extends Move {
    override def inverse: Move = upRightBy(distance)
  }

  object DownLeft extends MoveCompanion {
    override val unit: Move = DownLeft(1)
  }

  val downLeftBy: Int => Move = buildMove(DownLeft)

  val downLeft: Move = downLeftBy(1)

  case class Down(distance: Int) extends Move {
    override def inverse: Move = upBy(distance)
  }

  object Down extends MoveCompanion {
    override val unit: Move = Down(1)
  }

  val downBy: Int => Move = buildMove(Down)

  val down: Move = downBy(1)

  case class DownRight(distance: Int) extends Move {
    override def inverse: Move = upLeftBy(distance)
  }

  object DownRight extends MoveCompanion {
    override val unit: Move = DownRight(1)
  }

  val downRightBy: Int => Move = buildMove(DownRight)

  val downRight: Move = downRightBy(1)

  case class Right(distance: Int) extends Move {
    override def inverse: Move = leftBy(distance)
  }

  object Right extends MoveCompanion {
    override val unit: Move = Right(1)
  }

  val rightBy: Int => Move = buildMove(Right)

  val right: Move = rightBy(1)

  case class UpRight(distance: Int) extends Move {
    override def inverse: Move = downLeftBy(distance)
  }

  object UpRight extends MoveCompanion {
    override val unit: Move = UpRight(1)
  }

  val upRightBy: Int => Move = buildMove(UpRight)

  val upRight: Move = upRightBy(1)

  private def buildMove(mc: MoveCompanion)(distance: Int) =
    if (distance == 0) NoMove
    else if (distance == 1) mc.unit
    else mc.apply(distance)


}