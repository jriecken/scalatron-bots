package ca.jimr.scalatron.bot

import ca.jimr.scalatron.data._

class BrownianBot extends Bot {
  def respond = {
    case WelcomeCommand(name, apocalypse, round, maxslaves) =>
      Seq(StatusResponse("Warming Up..."))
    case ReactCommand(generation, name, time, view, energy, slaves, master, collision, state) =>
      Seq(MoveResponse(direction = Direction.random)) ++ (if (time % 100 == 0) Seq(SayResponse("Hmm...")) else Seq())
  }
}
