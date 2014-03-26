package ca.jimr.scalatron.api

/*
 * All the commands that we can send back to the server
 */

sealed trait BotCommand {
  protected def toParamStr(params: Map[String,String])  = {
    params.toSeq.map { case (k, v) => k+"="+v }.mkString(",")
  }
}

object BotCommand {
  /*
   * Responses that affect server state
   */

  case class Move(direction: Direction) extends BotCommand {
    override def toString = "Move(direction="+direction+")"
  }

  case class Spawn(direction: Direction, name: Option[String] = None,
                   energy: Option[Int] = None, state: Map[String,String] = Map()
  ) extends BotCommand {
    override def toString = {
      val paramMap = Map("direction" -> direction.toString) ++
        name.map(n => Map("name" -> n)).getOrElse(Map()) ++
        energy.map(e => Map("energy" -> e.toString)).getOrElse(Map()) ++
        state
      "Spawn("+toParamStr(paramMap)+")"
    }
  }

  case class SetState(state: Map[String,String] = Map()) extends BotCommand {
    override def toString = "Set("+toParamStr(state)+")"

    def merge(newState: Map[String,String]): SetState = {
      copy(state = state ++ newState)
    }

    def add(key: String, value: String): SetState = {
      copy(state = state + (key -> value))
    }

    def remove(key: String): SetState = {
      copy(state = state + (key -> ""))
    }
  }

  case class Explode(size: Int) extends BotCommand {
    override def toString = "Explode(size="+size+")"
  }

  /*
   * Responses that do not affect server state
   */

  case class Say(text: String) extends BotCommand {
    override def toString = "Say(text="+text+")"
  }

  case class Status(text: String) extends BotCommand {
    override def toString = "Status(text="+text+")"
  }

  case class MarkCell(position: Position = Position(0,0), color: String = "#8888ff") extends BotCommand {
    override def toString = "MarkCell(position="+position+",color="+color+")"
  }

  case class Log(text: String = "") extends BotCommand {
    override def toString = "Log(text="+text+")"

    def append(msg: String): Log = {
      copy(text = text + "\n" + msg)
    }
  }
}
