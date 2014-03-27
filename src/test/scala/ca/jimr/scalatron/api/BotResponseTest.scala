package ca.jimr.scalatron.api

import BotCommand._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class BotResponseTest extends WordSpec with ShouldMatchers {
  "merge" must {
    "merge together multiple responses" in {
      val resp = BotResponse().withState("one", "value")
      val resp2  = BotResponse().withMove(Direction.North)
      val merged = BotResponse().merge(resp, resp2)
      merged.cmds should equal(Map("Set" -> SetState(Map("one" -> "value")), "Move" -> Move(Direction.North)))
    }

    "merge set and log correctly" in {
      val resp = BotResponse().withState(Map("one" -> "value", "two" -> "value")).withLogging("One")
      val resp2 = BotResponse().withState(Map("two" -> "newValue", "three" -> "value")).withLogging("Two")
      val merged = BotResponse().merge(resp, resp2)
      merged.cmds should equal(Map("Set" -> SetState(Map("one" -> "value", "two" -> "newValue", "three" -> "value")), "Log" -> Log("One\nTwo")))
    }

    "merge with ++" in {
      val resp = BotResponse().withState("one", "value")
      val resp2  = BotResponse().withMove(Direction.North)
      val merged = resp ++ resp2
      merged.cmds should equal(Map("Set" -> SetState(Map("one" -> "value")), "Move" -> Move(Direction.North)))
    }
  }

  "withMove" must {
    "add a move cmd" in {
      val resp = BotResponse()
      val result = resp.withMove(Direction.North)
      result.cmds should equal(Map("Move" -> Move(Direction.North)))
    }
  }

  "withSpawn" must {
    "add a spawn command" in {
      val resp = BotResponse()
      val result = resp.withSpawn(Spawn(Direction.North, energy = Some(200)))
      result.cmds should equal(Map("Spawn" -> Spawn(Direction.North, energy = Some(200))))
    }
  }

  "withState" must {
    "add new state" in {
      val resp = BotResponse()
      val result = resp.withState(Map("one" -> "value"))
      result.cmds should equal(Map("Set" -> SetState(Map("one" -> "value"))))
    }

    "add a single new state" in {
      val resp = BotResponse()
      val result = resp.withState("one", "value")
      result.cmds should equal(Map("Set" -> SetState(Map("one" -> "value"))))
    }

    "overwrite existing state" in {
      val resp = BotResponse()
      val result = resp.withState(Map("one" -> "value")).withState(Map("one" -> "newValue"))
      result.cmds should equal(Map("Set" -> SetState(Map("one" -> "newValue"))))
    }

    "overwrite a single value" in {
      val resp = BotResponse()
      val result = resp.withState("one", "value").withState("one", "newValue")
      result.cmds should equal(Map("Set" -> SetState(Map("one" -> "newValue"))))
    }
  }

  "withoutState" must {
    "remove state" in {
      val resp = BotResponse()
      val result = resp.withState(Map("one" -> "value", "two" -> "value")).withoutState("one", "two")
      result.cmds should equal(Map("Set" -> SetState(Map("one" -> "", "two" -> ""))))
    }
  }

  "withExplode" must {
    "add an explode cmd" in {
      val resp = BotResponse()
      val result = resp.withExplode(10)
      result.cmds should equal(Map("Explode" -> Explode(10)))
    }
  }

  "withSay" must {
    "add a say cmd" in {
      val resp = BotResponse()
      val result = resp.withSay("Hello")
      result.cmds should equal(Map("Say" -> Say("Hello")))
    }
  }

  "withStatus" must {
    "add a status cmd" in {
      val resp = BotResponse()
      val result = resp.withStatus("Hello")
      result.cmds should equal(Map("Status" -> Status("Hello")))
    }
  }

  "withMarkCell" must {
    "add a markcell cmd" in {
      val resp = BotResponse()
      val result = resp.withMarkCell(MarkCell(Position(10,10), "#FF0000"))
      result.cmds should equal(Map("MarkCell" -> MarkCell(Position(10,10), "#FF0000")))
    }
  }

  "withLogging" must {
    "add a new log" in {
      val resp = BotResponse()
      val result = resp.withLogging("Hello")
      result.cmds should equal(Map("Log" -> Log("Hello")))
    }

    "append to existing log" in {
      val resp = BotResponse()
      val result = resp.withLogging("Hello").withLogging("World")
      result.cmds should equal(Map("Log" -> Log("Hello\nWorld")))
    }
  }

  "toString" must {
    "put all commands together" in {
      val str = BotResponse().withMove(Direction.South).withState("foo", "bar").toString
      str should equal("Move(direction=0:1)|Set(foo=bar)")
    }
  }
}
