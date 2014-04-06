package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api.Entity._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api._
import scala.util.Random

trait CommonBehavior {
  // The default "end time" of the match if we don't get a welcome message
  val DefaultEndTime = 5000

  // Tell the bot to move in a random direction until it is going to hit something, then
  // tells it to change direction
  def explore(resp: BotResponse)(implicit cmd: React): BotResponse = {
    cmd.state.get("lastMoved").map(d => Direction(d)).flatMap { dir =>
      if (okToMoveInDirection(dir)) {
        Some(resp.withMove(dir))
      } else {
        None
      }
    }.orElse {
      randomOkDirection().map(d => resp.withMove(d).withState("lastMoved", d.toString))
    }.getOrElse {
      resp
    }
  }

  def findFood(resp: BotResponse)(implicit cmd: React): BotResponse = {
    findFood(resp, avoidMaster = false, preferMaster = false)
  }

  // Go towards food, or explore if there is no food
  def findFood(resp: BotResponse, avoidMaster: Boolean = false, preferMaster: Boolean = false)(implicit cmd: React): BotResponse = {
    val dir = if (preferMaster) {
      directionTowardsClosest(Entity.isMeMaster).orElse(directionTowardsClosest(Entity.isFood))
    } else {
      directionTowardsClosest(Entity.isFood, avoidMaster)
    }
    dir.map(resp.withMove).getOrElse(explore(resp))
  }

  // Tells the bot to move to the most appealing direction based on what it can see
  // Note, this can sometimes make the bot super indecisive and cause it to oscillate
  // between two positions for a long time.
  def moveInAppealingDirection(resp: BotResponse)(implicit cmd: React): BotResponse = {
    if (!resp.cmds.contains("Spawn")) {
      val lastMoved = cmd.state.get("lastMoved").map(d => Direction(d))
      val closestFood = cmd.view.filterEntitiesPos(Entity.isFood).map(p => Direction(p))
      val (dir, _) = cmd.view.entities.filterNot {
        case (entity, _) => entity == Me
      }.map { case (entity, position) =>
        val distance = position.steps
        val score = if (entity == Enemy && distance < 2) {
          -1000
        } else if (entity == MiniEnemy) {
          -100.0 / distance
        } else if (entity == MiniMe) {
          -100.0 / distance
        } else if (entity == Fluppet) {
          distance match {
            case 1 => 600
            case 2 => 300
            case _ => (150 - distance * 15).max(10)
          }
        } else if (entity == Zugar) {
          distance match {
            case 1 => 500
            case 2 => 300
            case _ => (150 - distance * 10).max(10)
          }
        } else if (entity == Snorg) {
          distance match {
            case x if x < 4 => -300.0 / distance
            case _ => -50.0 / distance
          }
        } else if (entity == Toxifera && distance < 2) {
          -1000
        } else if (entity == Wall && distance < 2) {
          -1000
        } else {
          0.0
        }
        (Direction(position), score)
      }.groupBy {
        case (d,_) => d
      }.map {
        case (d, vs) =>
          val sumOfScores = vs.map { case (_, s) => s}.sum
          val bonusOrPenalty = Seq(
            // Give the direction with the closest food item more weight
            (closestFood.exists(_ == d), 10),
            // Give the last moved direction more weight
            (lastMoved.exists(_ == d), 10),
            // Flip-flopping directions is discouraged
            (lastMoved.exists(d => Direction(d.toPosition.reverse) == d), -50)
          ).filter { case (t, _) => t }.collect { case (_, v) => v }.sum
          (d, sumOfScores + bonusOrPenalty)
      }.toSeq.maxBy {
        case (_, score) => score
      }
      if (okToMoveInDirection(dir)) {
        resp.withMove(dir).withState("lastMoved", dir.toString)
      } else {
        randomOkDirection().map(resp.withMove).getOrElse(resp)
      }
    } else {
      resp
    }
  }

  def directionTowardsClosest(test: (Entity) => Boolean, avoidMaster: Boolean = false)(implicit cmd: React): Option[Direction] = {
    cmd.view.closestPosition(cmd.view.filterEntitiesPos(test)).flatMap(directionTowards(_, avoidMaster))
  }

  def directionTowards(rel: Position, avoidMaster: Boolean = false)(implicit cmd: React): Option[Direction] = {
    val dir = Direction(rel)
    if (okToMoveInDirection(dir, avoidMaster)) {
      Some(dir)
    } else {
      if (rel.y != 0 && okToMoveInDirection(Direction(Position(0, rel.y)), avoidMaster)) {
        Some(Direction(Position(0, rel.y)))
      } else if (rel.x != 0 && okToMoveInDirection(Direction(Position(rel.x, 0)), avoidMaster)) {
        Some(Direction(Position(rel.x, 0)))
      } else {
        None
      }
    }
  }

  def randomOkDirection(avoidMaster: Boolean = false)(implicit cmd: React): Option[Direction] = {
    Random.shuffle(Direction.All).find(okToMoveInDirection(_, avoidMaster))
  }

  def okToMoveInDirection(dir: Direction, avoidMaster: Boolean = false)(implicit cmd: React): Boolean = {
    val adjEntity = cmd.view.entityRelative(dir.toPosition)
    adjEntity != Wall && adjEntity != Toxifera &&
    adjEntity != Snorg && adjEntity != Enemy &&
      (!avoidMaster || adjEntity != Me)
  }
}
