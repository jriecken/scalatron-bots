package ca.jimr.scalatron.api

/*
 * All the game entities
 */

abstract class Entity(val character: Char, val travelCost: Int) {
  import Entity._

  def isEdible: Boolean = this == Zugar || this == Fluppet
  def isEnemy: Boolean = this == Enemy || this == MiniEnemy
  def isAttackable: Boolean = isEnemy || this == Snorg
  def isEvil: Boolean = isEnemy || this == Toxifera || this == Snorg || this == Wall
}

object Entity {
  case object Unknown extends Entity('?', 1000)
  case object Empty extends Entity('_', 100)
  case object Wall extends Entity('W', 1000000)

  case object Me extends Entity('M', 1000000)
  case object MiniMe extends Entity('S', 1000000)
  case object Enemy extends Entity('m', 1000000)
  case object MiniEnemy extends Entity('s', 1000000)

  case object Zugar extends Entity('P', 0)
  case object Toxifera extends Entity('p', 1000000)

  case object Fluppet extends Entity('B', 10)
  case object Snorg extends Entity('b', 1000000)

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