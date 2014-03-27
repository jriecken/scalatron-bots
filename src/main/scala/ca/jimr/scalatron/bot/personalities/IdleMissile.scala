package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for an "Idle" missile (a.k.a a smart-mine)
 *
 * - Sits in one spot unless
 *   - Enemy master comes within view => becomes OffensiveMissile
 *   - Enemy slave comes within 2 units => becomes DefensiveMissile
 *   - Snorg gets withing 5 units => starts evading snorg (but still looks for enemies)
 */
object IdleMissile extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      resp.withStatus("IdleMissile")
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
