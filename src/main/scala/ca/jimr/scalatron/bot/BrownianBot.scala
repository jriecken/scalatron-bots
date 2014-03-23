package ca.jimr.scalatron.bot

import ca.jimr.scalatron.data._
import ca.jimr.scalatron.data.BotResponse._
import ca.jimr.scalatron.data.ServerCommand._
import scala.util.Random

class BrownianBot extends Bot {
  def respond = {
    case r: React =>
      (r.generation, r.state.get("type").getOrElse("master")) match {
        case (0, "master") => controlMaster(r)
        case (1, "missile") => controlMissile(r)
        case _ => List() // Unknown bot
      }
  }

  def controlMaster(cmd: React): List[BotResponse] = {
    moveMaster(cmd.view, cmd.state) ++
      attackMaster(cmd.view, cmd.state, cmd.time, cmd.energy)
  }

  def controlMissile(cmd: React): List[BotResponse] = {
    val view = cmd.view
    val targets = view.filterEntitiesPos(_.isAttackable)
    view.closestPosition(targets).flatMap { pos =>
      val distance = pos.length
      if (distance <= 2) {
        Some(List(Say("Boom"),Explode(size = 5)))
      } else {
        None
      }
    }.getOrElse {
      seekEnemy(view, targets, cmd.state)
    }
  }

  def seekEnemy(view: BotView, targets: Seq[Position], state: Map[String,String]) = {
    val target = view.closestPosition(targets).getOrElse(Position(state("target")))
    List(Move(Direction(target)))
  }

  def attackMaster(view: BotView, state: Map[String,String], time: Int, energyLeft: Int) = {
    val targets = view.filterEntitiesPos(_.isEnemy)
    val lastMissileFired = state.get("lastMissileFired").map(_.toInt).getOrElse(0)
    view.closestPosition(targets).flatMap { target =>
      val idealPower = 100 + target.length // Payload + Fuel to get there
      val actualPower = if (energyLeft >= idealPower) idealPower else 100
      if ((time - lastMissileFired) > 10 && energyLeft >= actualPower) {
        // Pew pew
        Some(List(
          SetState(Map(
            "lastMissileFired" -> time.toString
          )),
          Spawn(
            direction = Direction(target),
            energy = Some(actualPower),
            state = Map(
              "type" -> "missile",
              "target" -> target.toString
            )
          )
        ))
      } else {
        // Not enough power
        None
      }
    }.getOrElse(List())
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
          Some(List(Move(dir), SetState(Map("moveDir" -> dir.toString))))
        }
//    }
    }.orElse {
      // Or move in our current direction until we hit something
      state.get("moveDir").map(d => Direction(d)).flatMap {
        dir =>
          val entityNext = view.entityRelative(dir.toPosition)
          if (entityNext == Entity.Empty || entityNext.isEdible) {
            // Continue moving in the direction we're moving in
            Some(List(Move(dir)))
          } else {
            None
          }
      }
    }.orElse {
      // Otherwise move in a random direction and remember that direction
      randomOkDirection(view).map(d => List(Move(d), SetState(Map("moveDir" -> d.toString))))
    }.getOrElse {
      // Trapped
      List()
    }
  }

  def randomOkDirection(view: BotView) = {
    Random.shuffle(Direction.All).
      map { dir => (dir, view.entityRelative(dir.toPosition)) }.
      find { case (_, e) => !e.isEvil }.
      map { case (dir, _) => dir }
  }
}
