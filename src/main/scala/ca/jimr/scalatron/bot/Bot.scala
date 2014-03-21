package ca.jimr.scalatron.bot

import ca.jimr.scalatron.data._

/**
 * Base bot class - All bot impls must extend this.
 *
 * Does marshalling/unmarshalling of commands/responses as well as storing the apocalypse/round/maxslaves on Welcome.
 */
abstract class Bot {
  /**
   * Implement this. Only need to handle ReactCommand. The others are optional.
   */
  def respond: PartialFunction[ServerCommand, Seq[BotResponse]]

  final def respondInternal(input: String): String = {
    val res = ServerCommand(input) match {
      case wc @ WelcomeCommand(name, apocalypse, round, maxslaves) =>
        val welcomeResp = Seq(SetResponse(Map("apocalypse" -> apocalypse.toString, "round" -> round.toString, "maxslaves" -> maxslaves.toString)))
        if (respond.isDefinedAt(wc)) {
          respond(wc) ++ welcomeResp
        } else {
          welcomeResp
        }
      case rc: ReactCommand => respond(rc)
      case gb: GoodbyeCommand =>
        if (respond.isDefinedAt(gb)) {
          respond(gb)
        } else {
          Seq()
        }
    }
    res.map(_.toString).mkString("|")
  }

  protected def apocalypse(state: Map[String, String]) = {
    state("apocalypse").toInt
  }

  protected def round(state: Map[String, String]) = {
    state("round").toInt
  }

  protected def maxslaves(state: Map[String, String]) = {
    state("maxslaves").toInt
  }


}
