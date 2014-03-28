package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.Entity._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import ca.jimr.scalatron.bot.personalities.DefensiveMissile._
import ca.jimr.scalatron.bot.personalities.Harvester._
import ca.jimr.scalatron.bot.personalities.IdleMissile._
import ca.jimr.scalatron.bot.personalities.OffensiveMissile._


/**
 * Behavior for master bot
 */
object Master extends Bot with CommonBehavior {
  object Constants {
    val InitialHarvesterOffset = 0
    val HarvesterLaunchInterval = 100
    val HarvestingPeriod = 50
    val HarvesterEnergy = 100
    val MasterIdleAfterLaunchHarvester = 5

    val OffensiveMissileEnergy = 200
    val DefensiveMissileEnergy = 100

    val InitialMineOffset = 50
    val MineLaunchInterval = 100
    val MineEnergy = 100
  }

  def respond = {
    case (cmd: React, resp: BotResponse) =>
      implicit val c = cmd
      Seq(
        fireDefensiveMissile _,
        fireOffensiveMissile _,
        launchHarvester _,
        //dropMine _, // Temporarily turning off mine laying as it doesn't seem to be helping...
        moveInAppealingDirection _
      ).foldLeft(resp) { (resp, fn) =>
        resp ++ fn(resp)
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

  def fireDefensiveMissile(resp: BotResponse)(implicit cmd: React): BotResponse = {
    val view = cmd.view
    if (!resp.cmds.contains("Spawn")) {
      val launchDefensiveMissileAfter = cmd.state.get("launchDefensiveMissileAfter").map(_.toInt).getOrElse(0)
      view.closestPosition(view.filterEntitiesPos(_ == MiniEnemy)).flatMap { pos =>
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
      view.filterEntitiesPos(_ == Enemy).headOption.map { pos =>
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

  def launchHarvester(resp: BotResponse)(implicit cmd: React): BotResponse = {
    if (!resp.cmds.contains("Spawn")) {
      val launchHarvesterAfter = cmd.state.get("launchNextHarvesterAfter").map(_.toInt).getOrElse(Constants.InitialHarvesterOffset)
      val okLaunchDirection = randomOkDirection()
      if (cmd.time > launchHarvesterAfter && cmd.energy > Constants.HarvesterEnergy && okLaunchDirection.isDefined) {
        resp.withState(Map(
          "launchNextHarvesterAfter" -> (cmd.time + Constants.HarvesterLaunchInterval).toString,
          "idleUntil" -> (cmd.time + Constants.MasterIdleAfterLaunchHarvester).toString
        )).spawnHarvester(okLaunchDirection.get, cmd.time + Constants.HarvestingPeriod, Some(Constants.HarvesterEnergy))
      } else {
        resp
      }
    } else {
      resp
    }
  }

  def moveInAppealingDirection(resp: BotResponse)(implicit cmd: React): BotResponse = {
    val idleUntil = cmd.state.get("idleUntil").map(_.toInt).getOrElse(0)
    if (!resp.cmds.contains("Spawn") && cmd.time > idleUntil) {
      val lastMoved = cmd.state.get("lastMoved").map(d => Direction(d))
      val closestFood = cmd.view.filterEntitiesPos(e => e == Fluppet || e == Zugar).map(p => Direction(p))
      val (dir, _) = cmd.view.entities.filterNot {
        case (entity, _) => entity == Me
      }.map { case (entity, position) =>
        val distance = position.steps
        val score = if (entity == Enemy && distance < 2) {
          -1000
        } else if (entity == MiniEnemy) {
          -100.0 / distance
        } else if (entity == MiniMe) {
          -100.0 / distance
        } else if (entity == Fluppet) {
          distance match {
            case 1 => 600
            case 2 => 300
            case _ => (150 - distance * 15).max(10)
          }
        } else if (entity == Zugar) {
          distance match {
            case 1 => 500
            case 2 => 300
            case _ => (150 - distance * 10).max(10)
          }
        } else if (entity == Snorg) {
          distance match {
            case x if x < 4 => -300.0 / distance
            case _ => -50.0 / distance
          }
        } else if (entity == Toxifera && distance < 2) {
          -1000
        } else if (entity == Wall && distance < 2) {
          -1000
        } else {
          0.0
        }
        (Direction(position), score)
      }.groupBy {
        case (d,_) => d
      }.map {
        case (d, vs) =>
          val sumOfScores = vs.map { case (_, s) => s}.sum
          val bonusOrPenalty = Seq(
            // Give the direction with the closest food item more weight
            (closestFood.exists(_ == d), 10),
            // Give the last moved direction more weight
            (lastMoved.exists(_ == d), 10),
            // Flip-flopping directions is discouraged
            (lastMoved.exists(d => Direction(d.toPosition.reverse) == d), -50)
          ).filter { case (t, _) => t }.collect { case (_, v) => v }.sum
          (d, sumOfScores + bonusOrPenalty)
      }.toSeq.maxBy {
        case (_, score) => score
      }
      if (okToMoveInDirection(dir)) {
        resp.withMove(dir).withState("lastMoved", dir.toString)
      } else {
        randomOkDirection().map(resp.withMove).getOrElse(resp)
      }
    } else {
      resp
    }
  }
}
