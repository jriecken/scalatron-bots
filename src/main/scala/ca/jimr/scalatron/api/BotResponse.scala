package ca.jimr.scalatron.api

import BotCommand._

case class BotResponse(cmds: Map[String, BotCommand] = Map()) {
  def merge(others: BotResponse*): BotResponse = {
    others.foldLeft(this) { (r, o) =>
      val simpleMerged = r.cmds ++ (o.cmds -- Seq("Set", "Log"))
      val setMerged = (r.cmds.get("Set"), o.cmds.get("Set")) match {
        case (None, None) => simpleMerged
        case (Some(_), None) => simpleMerged
        case (None, Some(s)) => simpleMerged + ("Set" -> s)
        case (Some(rs: SetState), Some(os: SetState)) => simpleMerged + ("Set" -> rs.merge(os.state))
        case _ => throw new IllegalArgumentException
      }
      val logMerged = (r.cmds.get("Log"), o.cmds.get("Log")) match {
        case (None, None) => setMerged
        case (Some(_), None) => setMerged
        case (None, Some(l)) => setMerged + ("Log" -> l)
        case (Some(rl: Log), Some(ol: Log)) => setMerged + ("Log" -> rl.append(ol.text))
        case _ => throw new IllegalArgumentException
      }
      r.copy(cmds = logMerged)
    }
  }

  def ++(other: BotResponse): BotResponse = {
    merge(other)
  }

  def withMove(direction: Direction): BotResponse = {
    copy(cmds + ("Move" -> Move(direction)))
  }

  def withSpawn(cmd: Spawn): BotResponse = {
    copy(cmds + ("Spawn" -> cmd))
  }

  def withState(state: Map[String,String]): BotResponse = {
    val newState = cmds.get("Set").orElse(Some(SetState())).map {
      case s: SetState => s.merge(state)
      case _ => throw new IllegalStateException
    }.get
    copy(cmds + ("Set" -> newState))
  }

  def withState(key: String, value: String): BotResponse = {
    val newState = cmds.get("Set").orElse(Some(SetState())).map {
      case s: SetState => s.add(key, value)
      case _ => throw new IllegalStateException
    }.get
    copy(cmds + ("Set" -> newState))
  }

  def withoutState(keys: String*): BotResponse = {
    val newState = cmds.get("Set").orElse(Some(SetState())).map {
      case s: SetState => keys.foldLeft(s) { (s, k) => s.remove(k)}
      case _ => throw new IllegalStateException
    }.get
    copy(cmds + ("Set" -> newState))
  }

  def withExplode(size: Int): BotResponse = {
    copy(cmds + ("Explode" -> Explode(size)))
  }

  def withSay(text: String): BotResponse = {
    copy(cmds + ("Say" -> Say(text)))
  }

  def withStatus(text: String): BotResponse = {
    copy(cmds + ("Status" -> Status(text)))
  }

  def withMarkCell(cmd: MarkCell): BotResponse = {
    copy(cmds + ("MarkCell" -> cmd))
  }

  def withLogging(text: String): BotResponse = {
    val newLog = cmds.get("Log").map {
      case l: Log => l.append(text)
      case _ => throw new IllegalStateException
    }.getOrElse(Log(text))
    copy(cmds + ("Log" -> newLog))
  }

  override def toString = {
    cmds.values.mkString("|")
  }
}
