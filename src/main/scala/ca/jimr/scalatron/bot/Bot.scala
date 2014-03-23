package ca.jimr.scalatron.bot

import ca.jimr.scalatron.data._
import ca.jimr.scalatron.data.BotResponse._
import ca.jimr.scalatron.data.ServerCommand._

/**
 * Base bot class - All bot impls must extend this.
 *
 * Does marshalling/unmarshalling of commands/responses as well as storing the apocalypse/round/maxslaves on Welcome.
 */
abstract class Bot {
  /**
   * Implement this. Only need to handle ReactCommand. The others are optional and handled for you.
   */
  def respond: PartialFunction[ServerCommand, List[BotResponse]]

  final def respondInternal(input: String): String = {
    try {
      // Allow the bot to handle the server request
      val res = ServerCommand(input) match {
        case wc @ Welcome(name, apocalypse, round, maxslaves) =>
          if (respond.isDefinedAt(wc)) {
            respond(wc)
          } else {
            List()
          }
        case rc: React =>
          respond(rc)
        case gb: Goodbye =>
          if (respond.isDefinedAt(gb)) {
            respond(gb)
          } else {
            List()
          }
      }
      // Merge together any SetState objects
      val (noState, allState) = res.partition {
        case SetState(_) => false
        case _ => true
      }
      val processedRes = if (allState.nonEmpty) {
        val mergedState = allState.foldLeft(Map[String,String]()) {
          case (m, SetState(state)) =>  m ++ state
          case (m, _) => m
        }
        SetState(mergedState) :: noState
      } else {
        noState
      }
      // Stringify the result
      processedRes.map(_.toString).mkString("|")
    } catch {
      case e: Exception =>
        e.printStackTrace()
        throw e
    }
  }
}
