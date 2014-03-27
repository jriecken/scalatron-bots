package ca.jimr.scalatron.api

import scala.util.Random
import java.net.NoRouteToHostException

/*
 * Cardinal directions
 */

trait Direction {

  import Direction._

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

object Direction {
  case object NorthWest extends Direction
  case object North extends Direction
  case object NorthEast extends Direction
  case object East extends Direction
  case object SouthEast extends Direction
  case object South extends Direction
  case object SouthWest extends Direction
  case object West extends Direction

  def apply(input: String): Direction = apply(Position(input))

  def apply(pos: Position): Direction = {
    val Position(x, y) = pos
    (x.signum, y.signum) match {
      case (-1, -1) if x < y * 3 => West
      case (-1, -1) if y < x * 3 => North
      case (-1, -1) => NorthWest
      case (-1, 0) => West
      case (-1, 1) if -x > y * 3 => West
      case (-1, 1) if y > -x * 3 => South
      case (-1, 1) => SouthWest
      case (0, 1) => South
      case (0, -1) => North
      case (1, -1) if x > -y * 3 => East
      case (1, -1) if -y > x * 3 => North
      case (1, -1) => NorthEast
      case (1, 0) => East
      case (1, 1) if x > y * 3 => East
      case (1, 1) if y > x * 3 => South
      case (1, 1) => SouthEast
      case _ => throw new IllegalArgumentException("Invalid direction")
    }
  }

  def random: Direction = All(Random.nextInt(8))

  val All = Vector(NorthWest, North, NorthEast, East, SouthEast, South, SouthWest, West)
}