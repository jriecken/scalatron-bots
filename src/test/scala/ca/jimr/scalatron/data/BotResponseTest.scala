package ca.jimr.scalatron.data

import org.scalatest.{Matchers, WordSpec}

class BotResponseTest extends WordSpec with Matchers {
  "MoveResponse" must {
    "stringify" in {
      MoveResponse(direction = North).toString should equal("Move(direction=0:-1)")
    }
  }

  "SpawnResponse" must {
    "stringify with minimal" in {
      SpawnResponse(direction = North).toString should equal("Spawn(direction=0:-1)")
    }

    "stringify with all fields" in {
      val resp = SpawnResponse(direction = South, name = Some("ChildBot1"), energy = Some(100), state = Map("custom1" -> "value1"))
      resp.toString should equal("Spawn(direction=0:1,name=ChildBot1,energy=100,custom1=value1)")
    }
  }

  "SetResponse" must {
    "stringify" in {
      val resp = SetResponse(state = Map("custom1" -> "value1", "custom2" -> "value2"))
      resp.toString should equal("Set(custom1=value1,custom2=value2)")
    }
  }

  "ExplodeResponse" must {
    "stringify" in {
      ExplodeResponse(size = 5).toString should equal("Explode(size=5)")
    }
  }

  "SayResponse" must {
    "stringify" in {
      SayResponse(text = "Hello World").toString should equal("Say(text=Hello World)")
    }
  }

  "StatusResponse" must {
    "stringify" in {
      StatusResponse(text = "Hello World").toString should equal("Status(text=Hello World)")
    }
  }

  "MarkCellResponse" must {
    "stringify" in {
      MarkCellResponse(position = Position(4,4), color = "#FF0000").toString should equal("MarkCell(position=4:4,color=#FF0000)")
    }
  }

  "LogResponse" must {
    "stringify" in {
      LogResponse(text = "Hello World").toString should equal("Log(text=Hello World)")
    }
  }
}
