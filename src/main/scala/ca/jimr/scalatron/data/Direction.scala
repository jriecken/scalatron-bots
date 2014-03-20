package ca.jimr.scalatron.data

/*
 * Cardinal directions
 */

trait Direction

case object UpLeft extends Position(-1, -1) with Direction
case object Up extends Position(0, -1) with Direction
case object UpRight extends Position(1, -1) with Direction
case object Right extends Position(1, 0) with Direction
case object DownRight extends Position(1, 1) with Direction
case object Down extends Position(0, 1) with Direction
case object DownLeft extends Position(-1, 1) with Direction
case object Left extends Position(-1, 0) with Direction

object Direction {
  def apply(pos: Position) = (pos.x.signum, pos.y.signum) match {
    case (-1,-1) => UpLeft
    case (0, -1) => Up
    case (1, -1) => UpRight
    case (1, 0) => Right
    case (1, 1) => DownRight
    case (0, 1) => Down
    case (-1, 1) => DownLeft
    case (-1, 0) => Left
    case _ => throw new IllegalArgumentException("Invalid direction")
  }

  val All = Seq(UpLeft, Up, UpRight, Right, DownRight, Down, DownLeft, Left)
}