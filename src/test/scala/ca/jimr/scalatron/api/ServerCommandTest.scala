package ca.jimr.scalatron.api

import ca.jimr.scalatron.api.Direction._
import ca.jimr.scalatron.api.ServerCommand._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class ServerCommandTest extends WordSpec with ShouldMatchers {
  "apply" must {
    "throw an error if an unknown command is passed in" in {
      intercept[IllegalArgumentException] {
        ServerCommand("Foo(blah=blah)")
      }
    }

    "parse a Welcome command" in {
      val cmd = ServerCommand("Welcome(name=Bot,apocalypse=1000,round=1,maxslaves=5)")
      cmd should equal(Welcome(name = "Bot", apocalypse = 1000, round = 1, maxslaves = Some(5)))
    }

    "parse a React command with no master/collection/params" in {
      val cmd = ServerCommand("React(generation=0,name=Bot,time=1,view=____M____,energy=1000,slaves=0)")
      cmd should equal(React(generation = 0, name = "Bot", time = 1, view = BotView("____M____"), energy = 1000, slaves = 0))
    }

    "parse a React command with master/collection/params" in {
      val cmd = ServerCommand("React(generation=1,name=Bot1,time=1,view=____M____,energy=100,slaves=0,master=5:0,collision=0:-1,custom1=value1,custom2=value2)")
      cmd should equal(React(generation = 1, name = "Bot1", time = 1, view = BotView("____M____"), energy = 100, slaves = 0, master = Some(Position(5, 0)), collision = Some(North), state = Map("custom1" -> "value1", "custom2" -> "value2")))
    }

    "parse a Goodbye command" in {
      val cmd = ServerCommand("Goodbye(energy=1000)")
      cmd should equal(Goodbye(energy = 1000))
    }
  }
}
