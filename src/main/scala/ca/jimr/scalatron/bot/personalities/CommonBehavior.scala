package ca.jimr.scalatron.bot.personalities

import ca.jimr.scalatron.api._
import ca.jimr.scalatron.api.ServerCommand._
import ca.jimr.scalatron.api.Entity._
import scala.util.Random

trait CommonBehavior {
  def explore(implicit cmd: React): BotResponse = {
    cmd.state.get("exploreDirection").map(d => Direction(d)).flatMap { dir =>
      if (okToMoveInDirection(dir)) {
        Some(BotResponse().withMove(dir))
      } else {
        None
      }
    }.orElse {
      randomOkDirection.map(d => BotResponse().withMove(d).withState("exploreDirection", d.toString))
    }.getOrElse {
      BotResponse()
    }
  }

  def moveTowards(rel: Position)(implicit cmd: React): Direction = {
    val dir = Direction(rel)
    if (okToMoveInDirection(dir)) {
      dir
    } else {
      val dirNoX = Direction(Position(0, rel.y))
      val dirNoY = Direction(Position(rel.x, 0))
      if (okToMoveInDirection(dirNoX)) {
        dirNoX
      } else if (okToMoveInDirection(dirNoY)) {
        dirNoY
      } else {
        // Not really any choice but to collide with something
        dir
      }
    }
  }

  def randomOkDirection(implicit cmd: React): Option[Direction] = {
    Random.shuffle(Direction.All).find(okToMoveInDirection)
  }

  def okToMoveInDirection(dir: Direction)(implicit cmd: React): Boolean = {
    val adjEntity = cmd.view.entityRelative(dir.toPosition)
    adjEntity != Wall && adjEntity != Toxifera && adjEntity != Snorg
  }
}
