package ca.jimr.scalatron.bot

import ca.jimr.scalatron.data._
import ca.jimr.scalatron.data.BotResponse._
import ca.jimr.scalatron.data.ServerCommand._
import scala.util.Random

class BrownianBot extends Bot {
  def respond = {
    case Welcome(name, apocalypse, round, maxslaves) =>
      Seq(Status("Warming Up..."))
    case React(generation, name, time, view, energy, slaves, master, collision, state) =>
      val yummy = view.filterEntities(_.isEdible).map { case (_, pos) => pos }
      view.closestPosition(yummy) match {
        case Some(pos) =>
          val dir = view.directionTo(pos)
          if (view.entityRelative(dir.toPosition).isEvil) {
            seek(view)
          } else {
            Seq(Move(view.directionTo(pos)), Status("Hunting"))
          }
        case None =>
          seek(view)
      }
  }

  def seek(view: BotView) = {
    Random.shuffle(Direction.All).
      map { dir => (dir, view.entityRelative(dir.toPosition)) }.
      find { case (_, e) => !e.isEvil }.
      map { case (dir, _) =>
      Seq(Move(direction = dir), Status("Seeking"))
    }.getOrElse {
      Seq(Status("Trapped"))
    }
  }
}
