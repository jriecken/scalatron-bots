package ca.jimr.scalatron.data

import scala.util.Random

/*
 * Cardinal directions
 */

trait Direction {
  def toPosition = this match {
    case NorthWest => Position(-1, -1)
    case North => Position(0, -1)
    case NorthEast => Position(1, -1)
    case East => Position(1, 0)
    case SouthEast => Position(1, 1)
    case South => Position(0, 1)
    case SouthWest => Position(-1, 1)
    case West => Position(-1, 0)
  }

  override def toString = {
    toPosition.toString
  }
}

case object NorthWest extends Direction
case object North extends Direction
case object NorthEast extends Direction
case object East extends Direction
case object SouthEast extends Direction
case object South extends Direction
case object SouthWest extends Direction
case object West extends  Direction

object Direction {
  def apply(pos: Position) = (pos.x.signum, pos.y.signum) match {
    case (-1,-1) => NorthWest
    case (0, -1) => North
    case (1, -1) => NorthEast
    case (1, 0) => East
    case (1, 1) => SouthEast
    case (0, 1) => South
    case (-1, 1) => SouthWest
    case (-1, 0) => West
    case _ => throw new IllegalArgumentException("Invalid direction")
  }

  def random = All(Random.nextInt(8))

  val All = Vector(NorthWest, North, NorthEast, East, SouthEast, South, SouthWest, West)
}