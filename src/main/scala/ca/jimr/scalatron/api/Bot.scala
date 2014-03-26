package ca.jimr.scalatron.api

/**
 * Base bot trait. All bots must mix this in in order to be executable
 */
trait Bot {
  /**
   * Implement this. All the commands are technically optional, but React should always be handled.
   */
  def respond: PartialFunction[ServerCommand, BotResponse]

  final def executeCommand(input: String): String = {
    try {
      respond.orElse(emptyResponse)(ServerCommand(input)).toString
    } catch {
      case e: Exception =>
        e.printStackTrace()
        throw e
    }
  }

  private def emptyResponse: PartialFunction[ServerCommand, BotResponse] = {
    case _ => BotResponse()
  }
}
