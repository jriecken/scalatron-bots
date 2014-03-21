package scalatron.botwar.botPlugin

import ca.jimr.scalatron.bot.BrownianBot

class ControlFunctionFactory {
  def create: (String => String) = {
    val bot = new BrownianBot
    bot.respondInternal
  }
}
