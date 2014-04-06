package ca.jimr.scalatron.bot

import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._

trait PersonalityBot extends Bot {
  val initialPersonality: String
  val personalities: Map[String, Bot]

  override def respond = {
    case (cmd: React, resp: BotResponse) =>
      val personality = cmd.state.get("personality").getOrElse(initialPersonality)
      personalities.get(personality).map(_.respond(cmd, resp)).getOrElse(resp)
    case (cmd: ServerCommand, resp: BotResponse) =>
      personalities.get(initialPersonality).flatMap { p =>
        if (p.respond.isDefinedAt(cmd, resp)) {
          Some(p.respond(cmd, resp))
        } else {
          None
        }
      }.getOrElse(resp)
  }
}

object PersonalityBot {
  class PersonalityBotResponse(resp: BotResponse) {
    def withNewPersonality(personality: String): BotResponse = {
      resp.withState("personality", personality)
    }

    def spawnWithPersonality(personality: String, spawn: Spawn) = {
      resp.withSpawn(spawn.copy(state = spawn.state + ("personality" -> personality)))
    }
  }
  implicit def br2pbr(resp: BotResponse) = {
    new PersonalityBotResponse(resp)
  }
}
