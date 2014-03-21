package ca.jimr.scalatron.data

/*
 * All the game entities
 */

abstract class Entity(val character: Char)

sealed trait Bot
sealed trait Plant
sealed trait Animal
sealed trait Good
sealed trait Evil

case object Unknown extends Entity('?')
case object Empty extends Entity('_')
case object Wall extends Entity('W') with Evil

case object Me extends Entity('M') with Bot with Good
case object MiniMe extends Entity('S') with Bot with Good
case object Enemy extends Entity('m') with Bot with Evil
case object MiniEnemy extends Entity('s') with Bot with Evil

case object Zugar extends Entity('P') with Plant with Good
case object Toxifera extends Entity('p') with Plant with Evil

case object Fluppet extends Entity('B') with Animal with Good
case object Snorg extends Entity('b') with Animal with Evil

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