package ca.jimr.scalatron.bot

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import scala.util.Random

object BrownianBot extends Bot {
  def respond = {
    case (cmd: React, resp: BotResponse) =>
      (cmd.generation, cmd.state.get("type").getOrElse("master")) match {
        case (0, "master") => controlMaster(cmd)
        case (1, "missile") => controlMissile(cmd)
        case _ => resp // Unknown bot
      }
  }

  def controlMaster(cmd: React) = {
    BotResponse().merge(
      moveMaster(cmd.view, cmd.state),
      attackMaster(cmd.view, cmd.state, cmd.time, cmd.energy)
    ).withStatus("Brownie")
  }

  def controlMissile(cmd: React) = {
    val view = cmd.view
    val targets = view.filterEntitiesPos(e => e == Entity.Enemy || e == Entity.MiniEnemy)
    view.closestPosition(targets).flatMap { pos =>
      val entity = view(pos)
      if (entity == Entity.MiniEnemy) {
        Some(seekEnemy(view, Seq(pos), cmd.state))
      } else {
        val distance = pos.length
        if (distance <= 2) {
          Some(BotResponse().withSay("Boom").withExplode(4))
        } else {
          None
        }
      }
    }.getOrElse {
      seekEnemy(view, targets, cmd.state)
    }
  }

  def seekEnemy(view: BotView, targets: Seq[Position], state: Map[String,String]) = {
    val stateTarget = Position(state("target"))
    val dirTarget = Direction(view.closestPosition(targets).getOrElse(stateTarget))
    BotResponse().withMove(dirTarget).withState("target", (stateTarget - dirTarget.toPosition).toString)
  }

  def attackMaster(view: BotView, state: Map[String,String], time: Int, energyLeft: Int) = {
    val targets = view.filterEntitiesPos(e => e == Entity.Enemy || e == Entity.MiniEnemy)
    val dontFireUntil = state.get("dontFireUntil").map(_.toInt).getOrElse(0)
    view.closestPosition(targets).flatMap { target =>
      if (time > dontFireUntil && energyLeft > 100) {
        // Pew pew
        Some(
          BotResponse().
          withState("dontFireUntil", (time + target.steps).toString).
          withSpawn(Spawn(
            direction = Direction(target),
            energy = Some(100),
            state = Map(
              "type" -> "missile",
              "target" -> target.toString
            )
          ))
        )
      } else {
        // Not enough power
        None
      }
    }.getOrElse(BotResponse())
  }

  def moveMaster(view: BotView, state: Map[String,String]) = {
    seek(view, state)
  }

  def seek(view: BotView, state: Map[String, String]) = {
    // Run towards food
    val closestEdible = view.closestPosition(view.filterEntitiesPos(e => e == Entity.Fluppet || e == Entity.Zugar))
    closestEdible.flatMap { pos =>
      val dir = view.directionTo(pos)
      val adjacentEntity = view.entityRelative(dir.toPosition)
      if (adjacentEntity == Entity.Wall || adjacentEntity == Entity.Toxifera) {
        None
      } else {
        Some(BotResponse().withMove(dir).withState("moveDir", dir.toString))
      }
    }.orElse {
      // Or move in our current direction until we hit something
      state.get("moveDir").map(d => Direction(d)).flatMap {
        dir =>
          val entityNext = view.entityRelative(dir.toPosition)
          if (entityNext == Entity.Empty || entityNext == Entity.Fluppet || entityNext == Entity.Zugar) {
            // Continue moving in the direction we're moving in
            Some(BotResponse().withMove(dir))
          } else {
            None
          }
      }
    }.orElse {
      // Otherwise move in a random direction and remember that direction
      randomOkDirection(view).map(d => BotResponse().withMove(d).withState("moveDir", d.toString))
    }.getOrElse {
      // Trapped
      BotResponse()
    }
  }

  def randomOkDirection(view: BotView) = {
    Random.shuffle(Direction.All).
      map { dir => (dir, view.entityRelative(dir.toPosition)) }.
      find { case (_, e) => e != Entity.Enemy && e != Entity.Wall && e != Entity.Me && e != Entity.MiniMe && e != Entity.Toxifera && e != Entity.Snorg }.
      map { case (dir, _) => dir }
  }
}
