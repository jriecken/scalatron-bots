package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for an offensive missile (that will explode) aimed at an Enemy master bot
 *
 * - Is given a target at construction and will move in that direction until it sees an enemy master in its view
 *   after which it will directly target the master
 * - If it gets within 2 units of the master it will explode with a radius of 4
 * - If it loses sight of the master it will turn into a DefensiveMissile if there is a slave in its view, otherwise
 *   it will move to the original target at which point it will become an IdleMissile
 */
object OffensiveMissile extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      implicit val c = cmd
      val view = cmd.view
      val target = Position(cmd.state("target"))
      val seenEnemy = cmd.state.get("seenEnemy").exists(_.toBoolean)
      val enemyPosition = view.filterEntitiesPos(Entity.isEnemyMaster).headOption
      enemyPosition.flatMap { pos =>
        if (pos.steps < 2) {
          Some(resp.withExplode(4))
        } else {
          directionTowards(pos).map(d => resp.withMove(d).withState(Map(
            "target" -> (target - d.toPosition).toString,
            "seenEnemy" -> seenEnemy.toString
          )))
        }
      }.orElse {
        if (seenEnemy && view.filterEntitiesPos(Entity.isEnemyMini).nonEmpty) {
          Some(resp.withoutState("seenEnemy").withNewPersonality("DefensiveMissile"))
        } else {
          if (target == Position(0, 0)) {
             Some(resp.withoutState("seenEnemy").withNewPersonality("IdleMissile"))
          } else {
            directionTowards(target).
              map(d => resp.withMove(d).withState("target", (target - d.toPosition).toString))
          }
        }
      }.getOrElse(resp)
  }

  class OffensiveMissileBotResponse(resp: BotResponse) {
    def spawnOffensiveMissile(direction: Direction, initialTarget: Position, energy: Option[Int] = None) = {
      resp.spawnWithPersonality("OffensiveMissile", Spawn(
        direction = direction,
        energy = energy,
        state = Map("target" -> (initialTarget - direction.toPosition).toString)
      ))
    }
  }
  implicit def br2ombr(resp: BotResponse) = {
    new OffensiveMissileBotResponse(resp)
  }
}
