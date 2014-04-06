package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for an "Idle" missile (a.k.a a smart-mine)
 *
 * - Sits in one spot unless
 *   - Enemy master comes within view => becomes OffensiveMissile
 *   - Enemy slave comes within 3 units => becomes DefensiveMissile
 */
object IdleMissile extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      val view = cmd.view
      val enemyMaster = view.filterEntitiesPos(Entity.isEnemyMaster)
      val enemySlave = view.closestPosition(view.filterEntitiesPos(Entity.isEnemyMini))
      if (enemyMaster.nonEmpty) {
        resp.withNewPersonality("OffensiveMissile").withState("target", enemyMaster(0).toString())
      } else if (enemySlave.isDefined && enemySlave.get.steps < 4) {
        resp.withNewPersonality("DefensiveMissile").withState("target", enemySlave.get.toString())
      } else {
        resp
      }
  }

  class IdleMissileBotResponse(resp: BotResponse) {
    def spawnIdleMissile(direction: Direction, energy: Option[Int] = None) = {
      resp.spawnWithPersonality("IdleMissile", Spawn(direction = direction, energy = energy))
    }
  }
  implicit def br2imbr(resp: BotResponse) = {
    new IdleMissileBotResponse(resp)
  }
}
