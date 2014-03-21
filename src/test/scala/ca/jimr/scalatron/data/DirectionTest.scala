package ca.jimr.scalatron.data

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
      Direction(Position(-5,-10)) should equal(NorthWest)
      Direction(Position(0, -7)) should equal(North)
      Direction(Position(4, -90)) should equal(NorthEast)
      Direction(Position(7, 0)) should equal(East)
      Direction(Position(2, 9)) should equal(SouthEast)
      Direction(Position(0, 10)) should equal(South)
      Direction(Position(-100, 4)) should equal(SouthWest)
      Direction(Position(-12, 0)) should equal(West)
    }
  }
}
