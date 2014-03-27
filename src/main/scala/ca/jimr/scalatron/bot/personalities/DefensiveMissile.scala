package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for a Defensive missile that will try to ram into enemy slaves.
 *
 * - Is given a target at construction and will move in that direction until it sees an enemy slave in its view
 *   after which it will directly target the slave and run into it
 * - If it loses sight of the slave it will turn into an OffensiveMissile if there is a master in view, otherwise
 *   it will move to the original target at which point it will turn into an IdleMissile
 */
object DefensiveMissile extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      resp.withStatus("DefensiveMissile")
  }

  class DefensiveMissileBotResponse(resp: BotResponse) {
    def spawnDefensiveMissile(direction: Direction, initialTarget: Position, energy: Option[Int] = None) = {
      resp.spawnWithPersonality("DefensiveMissile", Spawn(
        direction = direction,
        energy = energy,
        state = Map("target" -> (initialTarget - direction.toPosition).toString)
      ))
    }
  }
  implicit def br2dmbr(resp: BotResponse) = {
    new DefensiveMissileBotResponse(resp)
  }
}
