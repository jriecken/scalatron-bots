package ca.jimr.scalatron.api

import ca.jimr.scalatron.api.Direction._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class DirectionTest extends WordSpec with ShouldMatchers {
  "toPosition" must {
    "handle North" in {
      NorthWest.toPosition should equal(Position(-1, -1))
      North.toPosition should equal(Position(0, -1))
      NorthEast.toPosition should equal(Position(1, -1))
      East.toPosition should equal(Position(1, 0))
      SouthEast.toPosition should equal(Position(1, 1))
      South.toPosition should equal(Position(0, 1))
      SouthWest.toPosition should equal(Position(-1, 1))
      West.toPosition should equal(Position(-1, 0))
    }
  }

  "apply" must {
    "work" in {
      Direction("0:1") should equal(South)
      Direction(Position(-10, -1)) should equal(West)
      Direction(Position(-1, -10)) should equal(North)
      Direction(Position(-1, -1)) should equal(NorthWest)
      Direction(Position(-10, 0)) should equal(West)
      Direction(Position(-10, 1)) should equal(West)
      Direction(Position(-1, 10)) should equal(South)
      Direction(Position(-1, 1)) should equal(SouthWest)
      Direction(Position(0, 10)) should equal(South)
      Direction(Position(0, -10)) should equal(North)
      Direction(Position(10, -1)) should equal(East)
      Direction(Position(1, -10)) should equal(North)
      Direction(Position(1, -1)) should equal(NorthEast)
      Direction(Position(10, 0)) should equal(East)
      Direction(Position(10, 1)) should equal(East)
      Direction(Position(1, 10)) should equal(South)
      Direction(Position(1, 1)) should equal(SouthEast)
      intercept[IllegalArgumentException] {
        Direction(Position(0,0))
      }
    }
  }
}
