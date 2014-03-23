package scalatron.botwar.botPlugin

import ca.jimr.scalatron.bot._

class ControlFunctionFactory {
  def create: (String => String) = {
    val bot = new BrownianBot
    bot.respondInternal
  }
}
