package ca.jimr.scalatron.data

/*
 * All the game entities
 */

abstract class Entity(val character: Char, val travelCost: Int)

sealed trait Bot
sealed trait Plant
sealed trait Animal
sealed trait Good
sealed trait Evil

case object Unknown extends Entity('?', 1000)
case object Empty extends Entity('_', 100)
case object Wall extends Entity('W', 1000000) with Evil

case object Me extends Entity('M', 1000000) with Bot with Good
case object MiniMe extends Entity('S', 1000000) with Bot with Good
case object Enemy extends Entity('m', 1000000) with Bot with Evil
case object MiniEnemy extends Entity('s', 1000000) with Bot with Evil

case object Zugar extends Entity('P', 0) with Plant with Good
case object Toxifera extends Entity('p', 1000000) with Plant with Evil

case object Fluppet extends Entity('B', 10) with Animal with Good
case object Snorg extends Entity('b', 1000000) with Animal with Evil

object Entity {
  def apply(input: Char) = input match {
    case '?' => Unknown
    case '_' => Empty
    case 'W' => Wall
    case 'M' => Me
    case 'm' => Enemy
    case 'S' => MiniMe
    case 's' => MiniEnemy
    case 'P' => Zugar
    case 'p' => Toxifera
    case 'B' => Fluppet
    case 'b' => Snorg
    case _ => throw new IllegalArgumentException("Invalid entity type")
  }
}