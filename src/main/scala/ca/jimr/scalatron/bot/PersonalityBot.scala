package ca.jimr.scalatron.bot

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.BotCommand._
import ca.jimr.scalatron.api.ServerCommand._

trait PersonalityBot extends Bot {
  val initialPersonality: String
  val personalities: Map[String, Bot]

  override def respond = {
    case r: React =>
      val personality = r.state.get("personality").getOrElse(initialPersonality)
      personalities.get(personality).map(_.respond(r)).getOrElse(BotResponse())
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
