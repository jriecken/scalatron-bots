package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import ca.jimr.scalatron.bot.PersonalityBot._

/**
 * Behavior for a Harvester that will target Fluppets and Zugar for a specified time frame,
 * after which it will start move back to the master and transfer energy back
 */
object Harvester extends Bot with CommonBehavior {
  object Constants {
    val Period = 50
    val GoHomePeriod = 75
    val MaxEnergy = 2000
  }

  def respond = {
    case (cmd: React, resp: BotResponse)  =>
      implicit val c = cmd
      Seq(
        harvest _
      ).foldLeft(resp) { (resp, fn) =>
        resp ++ fn(resp)
      }
  }

  def harvest(resp: BotResponse)(implicit cmd: React) = {
    val time = cmd.time
    val stopHarvestAtTime = cmd.state("stopHarvestAtTime").toInt
    val stopHarvestAtEnergy = cmd.state("stopHarvestAtEnergy").toInt
    val endTime = cmd.state("endTime").toInt
    if (time > (endTime - 150)) {
      goHome(resp)
    } else if (time < stopHarvestAtTime && cmd.energy < stopHarvestAtEnergy) {
      findFood(resp)
    } else {
      if (time - stopHarvestAtTime < Constants.GoHomePeriod) {
        goHome(resp)
      } else {
        findFood(resp).withState("stopHarvestingAtTime", (time + Constants.Period).toString)
      }
    }
  }

  def goHome(resp: BotResponse)(implicit cmd: React) = {
    directionTowards(cmd.master.get).filter(d => okToMoveInDirection(d)).map(resp.withMove).getOrElse(resp)
  }

  class HarvesterBotResponse(resp: BotResponse) {
    def spawnHarvester(direction: Direction, energy: Option[Int] = None)(implicit cmd: React) = {
      resp.spawnWithPersonality("Harvester", Spawn(
        direction = direction,
        energy = energy,
        state = Map(
          "stopHarvestAtTime" -> (cmd.time + Constants.Period).toString,
          "stopHarvestAtEnergy" -> Constants.MaxEnergy.toString,
          "endTime" -> cmd.state.get("endTime").getOrElse(DefaultEndTime.toString)
        )
      ))
    }
  }
  implicit def br2hbr(resp: BotResponse) = {
    new HarvesterBotResponse(resp)
  }
}
