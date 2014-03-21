package ca.jimr.scalatron.bot

import ca.jimr.scalatron.data._
import scala.util.Random

class BrownianBot extends Bot {
  def respond = {
    case WelcomeCommand(name, apocalypse, round, maxslaves) =>
      Seq(StatusResponse("Warming Up..."))
    case ReactCommand(generation, name, time, view, energy, slaves, master, collision, state) =>
      if (energy > 0) {
        Random.shuffle(Direction.All).
          map { dir => (dir, view.entityAtRelative(dir.toPosition)) }.
          find { case (_, entity) => !entity.isInstanceOf[Evil] }.
          map { case (dir, _) =>
          Seq(MoveResponse(direction = dir), StatusResponse("Moving...")) ++ (if (time % 100 == 0) Seq(SayResponse("Hmm...")) else Seq())
        }.getOrElse {
          Seq(StatusResponse("Trapped!"))
        }
      } else {
        Seq(StatusResponse("Oh Cruel World..."))
      }
  }
}
