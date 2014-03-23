package ca.jimr.scalatron.data

import ca.jimr.scalatron.data.Direction._

/*
 * Bot's current view of things
 */
case class BotView(state: String) {
  lazy val entities = {
    state.map(Entity(_)).zipWithIndex.map { case (e, idx) => (e, relativize(indexToPos(idx)))}
  }

  def apply(rel: Position) = entityRelative(rel)

  def entityRelative(rel: Position) = entityAt(center + rel)

  def entityAt(pos: Position) = {
    val (e, _) = entities(posToIndex(pos))
    e
  }

  def filterEntities(filter: (Entity) => Boolean) = {
    entities.filter { case (e, _) => filter(e) }
  }

  def filterEntitiesPos(filter: (Entity) => Boolean) = {
    filterEntities(filter).map { case (_, pos) => pos }
  }

  def closestPosition(positions: Seq[Position]) = {
    positions.sortBy(_.length).headOption
  }

  def directionTo(pos: Position) = {
    Direction(pos)
  }

  def shortestPathDirectionTo(pos: Position) = {
    // TODO: implement pathfinding
    North
  }

  // TODO: Finish Impl

  private val size = math.sqrt(state.length).toInt
  private val center = Position(size / 2, size / 2)

  private def relativize(pos: Position) = pos - center

  private def indexToPos(index: Int) = {
    Position(index % size, index / size)
  }

  private def posToIndex(pos: Position) = {
    if (pos.x < 0 || pos.y < 0 || pos.x > size || pos.y > size) {
      throw new IllegalArgumentException("Invalid position: "+pos)
    }
    size * pos.y + pos.x
  }
}
