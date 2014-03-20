package ca.jimr.scalatron.data

/*
 * Bot's current view of things
 */

case class BotView(state: String) {
  private val size = math.sqrt(state.length).toInt
  private val center = Position(size / 2, size / 2)
  def apply(rel: Position) = entityAtRelative(rel)

  def entityAtRelative(rel: Position) = entityAt(center + rel)

  def entityAt(pos: Position) = Entity(state.charAt(posToIndex(pos)))

  def entitiesOfType(entType: Entity) = {
    state.view.zipWithIndex.filter { case (ch, _) =>
      ch == entType.character
    }.map { case (_, idx) =>
      relativize(indexToPos(idx))
    }
  }

  private def relativize(pos: Position) = pos - center

  private def indexToPos(index: Int) = {
    Position(index % size, index / size)
  }

  private def posToIndex(pos: Position) = {
    if (pos.x < 0 || pos.y < 0 || pos.x > size || pos.y > size) {
      throw new IllegalArgumentException(s"Invalid position: $pos")
    }
    size * pos.y + pos.x
  }

  // TODO: Finish Impl
}
