package ca.jimr.scalatron.api

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class PositionTest extends WordSpec with ShouldMatchers {
  "reverse" should {
    "work" in {
      Position(1, 1).reverse should equal(Position(-1, -1))
    }
  }

  "+" should {
    "work" in {
      (Position(1, 1) + Position(2,2)) should equal(Position(3,3))
    }
  }

  "-" should {
    "work" in {
      (Position(1, 1) - Position(2,2)) should equal(Position(-1,-1))
    }
  }

  "length" should {
    "work" in {
      Position(3, 4).length.toInt should equal(5)
    }
  }

  "steps" should {
    "work" in {
      Position(3, -4).steps should equal(4)
    }
  }
}
