package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.Entity._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for a Harvester that will target Fluppets and Zugar for a specified time frame,
 * after which it will move back to the bot that spawned it and transfer the harvested energy
 */
object Harvester extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      implicit val c = cmd
      Seq(
        harvestFoodUntilLimit _,
        returnToBaseAfterLimit _
      ).foldLeft(resp) { (resp, fn) =>
        resp ++ fn(resp)
      }
  }

  def harvestFoodUntilLimit(resp: BotResponse)(implicit cmd: React) = {
    val time = cmd.time
    val harvestUntil = cmd.state("harvestUntil").toInt
    val harvestDelay = cmd.state("harvestDelay").toInt
    if (time > harvestDelay && time < harvestUntil) {
      directionTowardsClosest(e => e == Fluppet || e == Zugar, avoidMaster = true).
        map(resp.withMove).
        getOrElse(resp).
        withStatus(cmd.energy.toString)
    } else {
      resp
    }
  }

  def returnToBaseAfterLimit(resp: BotResponse)(implicit cmd: React) = {
    val time = cmd.time
    val harvestUntil = cmd.state("harvestUntil").toInt
    if (time >= harvestUntil) {
      directionTowards(cmd.master.get).
        map(resp.withMove).
        getOrElse(resp).
        withStatus("Full")
    } else {
      resp
    }
  }

  class HarvesterBotResponse(resp: BotResponse) {
    def spawnHarvester(direction: Direction, harvestUntil: Int, energy: Option[Int] = None) = {
      resp.spawnWithPersonality("Harvester", Spawn(
        direction = direction,
        energy = energy,
        state = Map("harvestUntil" -> harvestUntil.toString, "harvestDelay" -> 10.toString)
      ))
    }
  }
  implicit def br2hbr(resp: BotResponse) = {
    new HarvesterBotResponse(resp)
  }
}
