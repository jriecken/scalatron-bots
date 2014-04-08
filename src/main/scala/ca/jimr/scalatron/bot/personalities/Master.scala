package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import ca.jimr.scalatron.bot.personalities.DefensiveMissile._
import ca.jimr.scalatron.bot.personalities.Harvester._
import ca.jimr.scalatron.bot.personalities.IdleMissile._
import ca.jimr.scalatron.bot.personalities.OffensiveMissile._

/**
 * Behavior for master bot. Has various things it tries to do (some are disabled for now).
 *
 * Mainly it fires out harvesters to gather yummies and fires defensive missiles at nearby enemy mini-bots
 */
object Master extends Bot with CommonBehavior {
  object Constants {
    val HarvestingMinPeriod = 200
    val HarvesterEnergy = 100

    val OffensiveMissileEnergy = 200
    val DefensiveMissileEnergy = 100

    val InitialMineOffset = 50
    val MineLaunchInterval = 100
    val MineEnergy = 100
  }

  def respond = {
    case (Welcome(_, apocalypse, _, _), resp: BotResponse) =>
      resp.withState("endTime", apocalypse.toString)
    case (cmd: React, resp: BotResponse) =>
      implicit val c = cmd
      Seq(
        fireDefensiveMissile _,
        //fireOffensiveMissile _, // Temporarily turning this off as offense < getting food
        //dropMine _, // Temporarily turning off mine laying as it doesn't seem to be helping...
        launchHarvester _,
        //moveInAppealingDirection _
        //explore _
        findFood _
      ).foldLeft(resp) { (resp, fn) =>
        resp ++ fn(resp)
      }
  }

  def launchHarvester(resp: BotResponse)(implicit cmd: React): BotResponse = {
    val endTime = cmd.state.get("endTime").map(_.toInt).getOrElse(DefaultEndTime)
    if (cmd.time < (endTime - 100) && !resp.cmds.contains("Spawn")) {
      val okLaunchDirection = randomOkDirection()
      if (cmd.energy > Constants.HarvesterEnergy && okLaunchDirection.isDefined) {
        resp.spawnHarvester(okLaunchDirection.get, cmd.time + Constants.HarvestingMinPeriod, Some(Constants.HarvesterEnergy))
      } else {
        resp
      }
    } else {
      resp
    }
  }

  def fireDefensiveMissile(resp: BotResponse)(implicit cmd: React): BotResponse = {
    val view = cmd.view
    if (!resp.cmds.contains("Spawn")) {
      val launchDefensiveMissileAfter = cmd.state.get("launchDefensiveMissileAfter").map(_.toInt).getOrElse(0)
      view.closestPosition(view.filterEntitiesPos(Entity.isEnemyMini)).flatMap { pos =>
        if (cmd.time > launchDefensiveMissileAfter && cmd.energy > Constants.DefensiveMissileEnergy && pos.steps < 4) {
          Some(resp.withState(Map(
            "launchDefensiveMissileAfter" -> (cmd.time + pos.steps).toString
          )).spawnDefensiveMissile(Direction(pos), pos, Some(Constants.DefensiveMissileEnergy)))
        } else {
          None
        }
      }.getOrElse(resp)
    } else {
      resp
    }
  }

  def fireOffensiveMissile(resp: BotResponse)(implicit cmd: React): BotResponse = {
    val view = cmd.view
    if (!resp.cmds.contains("Spawn")) {
      val launchOffensiveMissileAfter = cmd.state.get("launchOffensiveMissileAfter").map(_.toInt).getOrElse(0)
      view.filterEntitiesPos(Entity.isEnemyMaster).headOption.map { pos =>
        if (cmd.time > launchOffensiveMissileAfter && cmd.energy > Constants.OffensiveMissileEnergy) {
          resp.withState(Map(
            "launchOffensiveMissileAfter" -> (cmd.time + pos.steps).toString
          )).spawnOffensiveMissile(Direction(pos), pos, Some(Constants.OffensiveMissileEnergy))
        } else {
          resp
        }
      }.getOrElse(resp)
    } else {
      resp
    }
  }

  def dropMine(resp: BotResponse)(implicit cmd: React): BotResponse = {
    if (!resp.cmds.contains("Spawn")) {
      val launchMineAfter = cmd.state.get("launchMineAfter").map(_.toInt).getOrElse(Constants.InitialMineOffset)
      val okLaunchDirection = randomOkDirection()
      if (cmd.time > launchMineAfter && cmd.energy > Constants.MineEnergy && okLaunchDirection.isDefined) {
        resp.withState(Map(
          "launchMineAfter" -> (cmd.time + Constants.MineLaunchInterval).toString
        )).spawnIdleMissile(okLaunchDirection.get, Some(Constants.MineEnergy))
      } else {
        resp
      }
    } else {
      resp
    }
  }
}
