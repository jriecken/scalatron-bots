package ca.jimr.scalatron.api

import ca.jimr.scalatron.api.Entity._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class EntityTest extends WordSpec with ShouldMatchers {
  "isEdible" must {
    "work" in {
      Unknown.isEdible should equal(false)
      Empty.isEdible should equal(false)
      Wall.isEdible should equal(false)
      Me.isEdible should equal(false)
      MiniMe.isEdible should equal(false)
      Enemy.isEdible should equal(false)
      MiniEnemy.isEdible should equal(false)
      Zugar.isEdible should equal(true)
      Toxifera.isEdible should equal(false)
      Fluppet.isEdible should equal(true)
      Snorg.isEdible should equal(false)
    }
  }

  "isEnemy" must {
    "work" in {
      Unknown.isEnemy should equal(false)
      Empty.isEnemy should equal(false)
      Wall.isEnemy should equal(false)
      Me.isEnemy should equal(false)
      MiniMe.isEnemy should equal(false)
      Enemy.isEnemy should equal(true)
      MiniEnemy.isEnemy should equal(true)
      Zugar.isEnemy should equal(false)
      Toxifera.isEnemy should equal(false)
      Fluppet.isEnemy should equal(false)
      Snorg.isEnemy should equal(false)
    }
  }

  "isAttackable" must {
    "work" in {
      Unknown.isAttackable should equal(false)
      Empty.isAttackable should equal(false)
      Wall.isAttackable should equal(false)
      Me.isAttackable should equal(false)
      MiniMe.isAttackable should equal(false)
      Enemy.isAttackable should equal(true)
      MiniEnemy.isAttackable should equal(true)
      Zugar.isAttackable should equal(false)
      Toxifera.isAttackable should equal(false)
      Fluppet.isAttackable should equal(false)
      Snorg.isAttackable should equal(true)
    }
  }

  "isEvil" must {
    "work" in {
      Unknown.isEvil should equal(false)
      Empty.isEvil should equal(false)
      Wall.isEvil should equal(true)
      Me.isEvil should equal(false)
      MiniMe.isEvil should equal(false)
      Enemy.isEvil should equal(true)
      MiniEnemy.isEvil should equal(true)
      Zugar.isEvil should equal(false)
      Toxifera.isEvil should equal(true)
      Fluppet.isEvil should equal(false)
      Snorg.isEvil should equal(true)
    }
  }

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
