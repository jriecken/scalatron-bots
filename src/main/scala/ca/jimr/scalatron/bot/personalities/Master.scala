package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api.Direction._
import ca.jimr.scalatron.api.Entity._
import ca.jimr.scalatron.bot.PersonalityBot._
import ca.jimr.scalatron.bot.personalities.IdleMissile._
import ca.jimr.scalatron.bot.personalities.OffensiveMissile._
import ca.jimr.scalatron.bot.personalities.DefensiveMissile._
import ca.jimr.scalatron.bot.personalities.Harvester._

/**
 * Behavior for master bot
 *
 * Bot moves around in the most "appealing" direction. This means it will:
 *  - Run away from Snorgs if they are within 5 units of the bot
 *  - Avoid moving into walls (i.e. if they are within 1 unit of the bot
 *  - Avoid moving into Toxifera (i.e. if they are within 1 unit of the bot
 *
 * Will:
 *  - Drop an IdleMissile every 500 time units (if it has > 500 energy)
 *  - Fire an OffensiveMissile if an enemy master comes within view (if has enough energy)
 *    Will wait to fire another missile until the missile has had time to move to where the enemy was when it fired
 *  - Fire a DefensiveMissile if an enemy slave comes within 5 units of the master
 *    Will wait to fire another defensive missile until the missile has had time to move to where the enemy slave was
 *    when it fired.
 *  - Launch a Harvester every 250 time units (if it has enough energy)
 */
object Master extends Bot with CommonBehavior {
  def respond = {
    case (cmd: React, resp: BotResponse) =>
      implicit val c = cmd
      resp.withStatus("Mistake Not...") ++ moveInAppealingDirection
  }

  def moveInAppealingDirection(implicit cmd: React): BotResponse = {
    val lastMoved = cmd.state.get("lastMoved").map(d => Direction(d))
    val (dir, _) = cmd.view.entities.filterNot {
      case (entity, _) => entity == Me
    }.map { case (entity, position) =>
      val distance = position.steps
      val score = if (entity == Enemy && distance < 2) {
        -1000
      } else if (entity == MiniEnemy) {
        -100.0 / distance
      } else if (entity == Fluppet) {
        distance match {
          case 1 => 1000
          case 2 => 500
          case _ => (250 - distance * 15).max(100)
        }
      } else if (entity == Zugar) {
        distance match {
          case 1 => 1200
          case 2 => 600
          case _ => (300 - distance * 15).max(100)
        }
      } else if (entity == Snorg) {
        distance match {
          case x if x < 4 => -400.0 / distance
          case _ => -50.0 / distance
        }
      } else if (entity == Toxifera && distance < 2) {
        -10000
      } else if (entity == Wall && distance < 2) {
        -10000
      } else {
        0.0
      }
      (Direction(position), score)
    }.groupBy {
      case (d,_) => d
    }.map {
      case (d, vs) =>
        val sumOfScores = vs.map { case (_, s) => s}.sum
        if (lastMoved.exists(_ == d)) {
          (d, sumOfScores + 10)
        } else {
          (d, sumOfScores)
        }
    }.toSeq.maxBy {
      case (_, score) => score
    }
    if (okToMoveInDirection(dir)) {
      BotResponse().withMove(dir).withState("lastMoved", dir.toString)
    } else {
      BotResponse()
    }
  }
}
