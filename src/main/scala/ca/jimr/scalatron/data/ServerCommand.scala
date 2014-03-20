package ca.jimr.scalatron.data

/*
 * All the commands that the server will send us
 */

sealed trait ServerCommand

case class WelcomeCommand(
  name: String,
  apocalypse: Int,
  round: Int,
  maxslaves: Int
) extends ServerCommand

case class ReactCommand(
  generation: Int,
  name: String,
  time: Int,
  view: BotView,
  energy: Int,
  slaves: Int,
  master: Option[Direction] = None,
  collision: Option[Direction] = None,
  props: Map[String,String] = Map()
) extends ServerCommand

case class GoodbyeCommand(energy: Int) extends ServerCommand

object ServerCommand {
  def apply(input: String) = {
    val tokens = input.split('(')
    val op = tokens(0)
    val params = tokens(1).dropRight(1).
      split(','). // Split into x=y params
      map(_.split('=')).map(a => a(0) -> a(1)). // Change into (x, y) pairs
      toMap // turn the pairs into a Map
    op match {
      case "Welcome" => WelcomeCommand(
        name = params("name"),
        apocalypse = params("apocalypse").toInt,
        round = params("round").toInt,
        maxslaves = params("maxslaves").toInt
      )
      case "Goodbye" => GoodbyeCommand(energy = params("energy").toInt)
      case "React" => ReactCommand(
        generation = params("generation").toInt,
        name = params("name"),
        time = params("time").toInt,
        view = BotView(input = params("view")),
        energy = params("energy").toInt,
        slaves = params("slaves").toInt,
        master = params.get("master").map(s => Direction(Position(s))),
        collision = params.get("collision").map(s => Direction(Position(s))),
        props = params -- Seq("generation", "name", "time", "view", "energy", "slaves", "master", "collision")
      )
    }
  }
}