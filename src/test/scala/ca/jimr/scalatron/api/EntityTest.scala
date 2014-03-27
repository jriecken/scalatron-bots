package ca.jimr.scalatron.api

import ca.jimr.scalatron.api.Entity._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class EntityTest extends WordSpec with ShouldMatchers {
  "apply" must {
    "throw an exception if an unknown char is passed in" in {
      intercept[IllegalArgumentException] {
        Entity('8')
      }
    }

    "work" in {
      Entity('?') should equal(Unknown)
      Entity('_') should equal(Empty)
      Entity('W') should equal(Wall)
      Entity('M') should equal(Me)
      Entity('m') should equal(Enemy)
      Entity('S') should equal(MiniMe)
      Entity('s') should equal(MiniEnemy)
      Entity('P') should equal(Zugar)
      Entity('p') should equal(Toxifera)
      Entity('B') should equal(Fluppet)
      Entity('b') should equal(Snorg)
    }
  }
}
