package ca.jimr.scalatron.data

/*
 * All the game entities
 */

sealed trait Entity

sealed trait Bot
sealed trait Plant
sealed trait Animal
sealed trait Good
sealed trait Evil

case object Unknown extends Entity
case object Empty extends Entity
case object Wall extends Entity with Evil

case object Me extends Entity with Bot with Good
case object MiniMe extends Entity with Bot with Good
case object Enemy extends Entity with Bot with Evil
case object MiniEnemy extends Entity with Bot with Evil

case object Zugar extends Entity with Plant with Good
case object Toxifera extends Entity with Plant with Evil

case object Fluppet extends Entity with Animal with Good
case object Snorg extends Entity with Animal with Evil

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
    case _ => throw new IllegalArgumentException(s"Invalid entity type")
  }
}