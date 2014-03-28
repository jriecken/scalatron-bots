package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api.Entity._
import scala.util.Random

trait CommonBehavior {
  def explore(resp: BotResponse)(implicit cmd: React): BotResponse = {
    cmd.state.get("exploreDirection").map(d => Direction(d)).flatMap { dir =>
      if (okToMoveInDirection(dir)) {
        Some(resp.withMove(dir))
      } else {
        None
      }
    }.orElse {
      randomOkDirection().map(d => resp.withMove(d).withState("exploreDirection", d.toString))
    }.getOrElse {
      resp
    }
  }

  def directionTowardsClosest(test: (Entity) => Boolean, avoidMaster: Boolean = false)(implicit cmd: React): Option[Direction] = {
    cmd.view.closestPosition(cmd.view.filterEntitiesPos(test)).
      flatMap(directionTowards(_, avoidMaster)).
      orElse(randomOkDirection(avoidMaster))
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

  def randomDirection(implicit cmd: React): Direction = {
    Random.shuffle(Direction.All).head
  }

  def randomOkDirection(avoidMaster: Boolean = false)(implicit cmd: React): Option[Direction] = {
    Random.shuffle(Direction.All).find(okToMoveInDirection(_, avoidMaster))
  }

  def okToMoveInDirection(dir: Direction, avoidMaster: Boolean = false)(implicit cmd: React): Boolean = {
    val adjEntity = cmd.view.entityRelative(dir.toPosition)
    adjEntity != Wall && adjEntity != Toxifera && adjEntity != Snorg && (!avoidMaster || adjEntity != Me)
  }
}
