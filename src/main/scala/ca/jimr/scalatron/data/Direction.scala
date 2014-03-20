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
  def apply(pos: Position) = pos match {
    case Position(x, y) if x < 0 && y < 0 => UpLeft
    case Position(x, y) if x == 0 && y < 0 => Up
    case Position(x, y) if x > 0 && y < 0 => UpRight
    case Position(x, y) if x > 0 && y == 0 => Right
    case Position(x, y) if x > 0 && y > 0 => DownRight
    case Position(x, y) if x == 0 && y > 0 => Down
    case Position(x, y) if x < 0 && y > 0 => DownLeft
    case Position(x, y) if x < 0 && y == 0 => Left
    case _ => throw new IllegalArgumentException("Invalid direction")
  }
}