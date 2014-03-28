package ca.jimr.scalatron.bot

import ca.jimr.scalatron.bot.personalities._

/**
 * M.O.N.A.D. - Might Overkill Nearly All Droids
 */
object MonadBot extends PersonalityBot {
  val initialPersonality = "Master"
  val personalities = Map(
    "Master" -> Master,
    "IdleMissile" -> IdleMissile,
    "OffensiveMissile" -> OffensiveMissile,
    "DefensiveMissile" -> DefensiveMissile,
    "Harvester" -> Harvester
  )
}
