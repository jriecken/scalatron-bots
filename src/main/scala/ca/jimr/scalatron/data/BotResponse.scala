package ca.jimr.scalatron.data

/*
 * All the responses that we can send back to the server
 *
 * Multiple responses can be returned, but only one of each type
 */

sealed trait BotResponse {
  protected def toParamStr(params: Map[String,String])  = {
    params.toSeq.map { case (k, v) => k+"="+v }.mkString(",")
  }
}

/*
 * Responses that affect server state
 */

case class MoveResponse(direction: Direction) extends BotResponse {
  override def toString = "Move(direction="+direction+")"
}

case class SpawnResponse(
  direction: Direction,
  name: Option[String] = None,
  energy: Option[Int] = None,
  state: Map[String,String] = Map()
) extends BotResponse {
  override def toString = {
    val paramMap = Map("direction" -> direction.toString) ++
      name.map(n => Map("name" -> n)).getOrElse(Map()) ++
      energy.map(e => Map("energy" -> e.toString)).getOrElse(Map()) ++
      state
    "Spawn("+toParamStr(paramMap)+")"
  }
}

case class SetResponse(state: Map[String,String]) extends BotResponse {
  override def toString = "Set("+toParamStr(state)+")"
}

case class ExplodeResponse(size: Int) extends BotResponse {
  override def toString = "Explode(size="+size+")"
}

/*
 * Responses that do not affect server state
 */

case class SayResponse(text: String) extends BotResponse {
  override def toString = "Say(text="+text+")"
}

case class StatusResponse(text: String) extends BotResponse {
  override def toString = "Status(text="+text+")"
}

case class MarkCellResponse(position: Position = Position(0,0), color: String = "#8888ff") extends BotResponse {
  override def toString = "MarkCell(position="+position+",color="+color+")"
}

case class LogResponse(text: String) extends BotResponse {
  override def toString = "Log(text="+text+")"
}