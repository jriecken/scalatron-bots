package ca.jimr.scalatron.api

/*
 * All the game entities
 */

abstract class Entity(val character: Char)

object Entity {
  case object Unknown extends Entity('?')
  case object Empty extends Entity('_')
  case object Wall extends Entity('W')

  case object Me extends Entity('M')
  case object MiniMe extends Entity('S')
  case object Enemy extends Entity('m')
  case object MiniEnemy extends Entity('s')

  case object Zugar extends Entity('P')
  case object Toxifera extends Entity('p')

  case object Fluppet extends Entity('B')
  case object Snorg extends Entity('b')

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

  // Filter operations

  def isMeMaster(e: Entity) = e == Me
  def isEnemyMaster(e: Entity) = e == Enemy
  def isEnemyMini(e: Entity) = e == MiniEnemy
  def isFood(e: Entity) = e == Fluppet || e == Zugar

}