package ca.jimr.scalatron.bot

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import scala.util.Random

class BrownianBot extends Bot {
  def respond = {
    case r: React =>
      (r.generation, r.state.get("type").getOrElse("master")) match {
        case (0, "master") => controlMaster(r)
        case (1, "missile") => controlMissile(r)
        case _ => BotResponse() // Unknown bot
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
    val targets = view.filterEntitiesPos(_.isAttackable)
    view.closestPosition(targets).flatMap { pos =>
      val distance = pos.length
      if (distance <= 2) {
        Some(BotResponse().withSay("Boom").withExplode(5))
      } else {
        None
      }
    }.getOrElse {
      seekEnemy(view, targets, cmd.state)
    }
  }

  def seekEnemy(view: BotView, targets: Seq[Position], state: Map[String,String]) = {
    val target = view.closestPosition(targets).getOrElse(Position(state("target")))
    BotResponse().withMove(Direction(target))
  }

  def attackMaster(view: BotView, state: Map[String,String], time: Int, energyLeft: Int) = {
    val targets = view.filterEntitiesPos(_.isEnemy)
    val lastMissileFired = state.get("lastMissileFired").map(_.toInt).getOrElse(0)
    view.closestPosition(targets).flatMap { target =>
      val idealPower = 100 + target.length // Payload + Fuel to get there
      val actualPower = if (energyLeft >= idealPower) idealPower else 100
      if ((time - lastMissileFired) > 10 && energyLeft >= actualPower) {
        // Pew pew
        Some(
          BotResponse().
          withState("lastMissileFired", time.toString).
          withSpawn(Spawn(
            direction = Direction(target),
            energy = Some(actualPower),
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
    //val closeSnorg = view.closestPosition(view.filterEntitiesPos(_ == Entity.Snorg)).filter(_.length < 5)
//    closeSnorg.flatMap { pos =>
//      // Run away from snorgs
//      val newDir = Direction(pos).reflect
//      if (view.entityRelative(newDir.toPosition).isEvil) {
//        None
//      } else {
//        Some(List(Move(newDir), SetState(Map("moveDir" -> newDir.toString))))
//      }
//    }.orElse {
      // Run towards food
      val closestEdible = view.closestPosition(view.filterEntitiesPos(_.isEdible))
      closestEdible.flatMap { pos =>
        val dir = view.directionTo(pos)
        if (view.entityRelative(dir.toPosition).isEvil) {
          None
        } else {
          Some(BotResponse().withMove(dir).withState("moveDir", dir.toString))
        }
//    }
    }.orElse {
      // Or move in our current direction until we hit something
      state.get("moveDir").map(d => Direction(d)).flatMap {
        dir =>
          val entityNext = view.entityRelative(dir.toPosition)
          if (entityNext == Entity.Empty || entityNext.isEdible) {
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
      find { case (_, e) => !e.isEvil }.
      map { case (dir, _) => dir }
  }
}
