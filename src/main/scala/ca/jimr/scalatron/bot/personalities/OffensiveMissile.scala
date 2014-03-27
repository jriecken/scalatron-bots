package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
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
      resp.withStatus("OffensiveMissle")
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
