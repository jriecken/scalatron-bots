package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for a Harvester that will target Fluppets and Zugar for a specified time frame,
 * after which it will move back to the bot that spawned it and transfer the harvested energy
 */
object Harvester extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      resp.withStatus("Harvester")
  }

  class HarvesterBotResponse(resp: BotResponse) {
    def spawnHarvester(direction: Direction, harvestUntil: Int, maxHarvestDistance: Int, energy: Option[Int] = None) = {
      resp.spawnWithPersonality("DefensiveMissile", Spawn(
        direction = direction,
        energy = energy,
        state = Map("harvestUntil" -> harvestUntil.toString, "maxHarvestDistance" -> maxHarvestDistance.toString)
      ))
    }
  }
  implicit def br2hbr(resp: BotResponse) = {
    new HarvesterBotResponse(resp)
  }
}
