package ca.jimr.scalatron.api

import ca.jimr.scalatron.api.Direction._
import ca.jimr.scalatron.api.BotCommand._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class BotCommandTest extends WordSpec with ShouldMatchers {
  "Move" must {
    "stringify" in {
      Move(direction = North).toString should equal("Move(direction=0:-1)")
    }
  }

  "Spawn" must {
    "stringify with minimal" in {
      Spawn(direction = North).toString should equal("Spawn(direction=0:-1)")
    }

    "stringify with all fields" in {
      val resp = Spawn(direction = South, name = Some("ChildBot1"), energy = Some(100), state = Map("custom1" -> "value1"))
      resp.toString should equal("Spawn(direction=0:1,name=ChildBot1,energy=100,custom1=value1)")
    }
  }

  "SetState" must {
    "stringify" in {
      val resp = SetState(state = Map("custom1" -> "value1", "custom2" -> "value2"))
      resp.toString should equal("Set(custom1=value1,custom2=value2)")
    }
  }

  "Explode" must {
    "stringify" in {
      Explode(size = 5).toString should equal("Explode(size=5)")
    }
  }

  "Say" must {
    "stringify" in {
      Say(text = "Hello World").toString should equal("Say(text=Hello World)")
    }
  }

  "Status" must {
    "stringify" in {
      Status(text = "Hello World").toString should equal("Status(text=Hello World)")
    }
  }

  "MarkCell" must {
    "stringify" in {
      MarkCell(position = Position(4,4), color = "#FF0000").toString should equal("MarkCell(position=4:4,color=#FF0000)")
    }
  }

  "Log" must {
    "stringify" in {
      Log(text = "Hello World").toString should equal("Log(text=Hello World)")
    }
  }
}
