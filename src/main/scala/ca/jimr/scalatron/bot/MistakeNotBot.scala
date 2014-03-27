package ca.jimr.scalatron.bot

import ca.jimr.scalatron.bot.personalities._

/**
 * Mistake Not... (My Current State Of Joshing Gentle Peevishness For The Awesome And Terrible
 * Majesty Of The Towering Seas Of Ire That Are Themselves The Milquetoast Shallows Fringing
 * My Vast Oceans Of Wrath)
 */
object MistakeNotBot extends PersonalityBot {
  val initialPersonality = "Master"
  val personalities = Map(
    "Master" -> Master,
    "IdleMissile" -> IdleMissile,
    "OffensiveMissile" -> OffensiveMissile,
    "DefensiveMissile" -> DefensiveMissile,
    "Harvester" -> Harvester
  )
}
