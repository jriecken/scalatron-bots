package ca.jimr.scalatron.data

/*
 * Bot's current view of things. The bot is always at the center of the
 * view.
 */

case class BotView(state: String) {
  private val size = math.sqrt(state.length).toInt
  private val center = Position(size / 2, size / 2)
  def apply(rel: Position) = entityAtRelative(rel)

  def entityAtRelative(rel: Position) = entityAt(center.add(rel))

  def entityAt(pos: Position) = Entity(state.charAt(posToIndex(pos)))

  private def posToIndex(pos: Position) = {
    if (pos.x < 0 || pos.y < 0 || pos.x > size || pos.y > size) {
      throw new IllegalArgumentException(s"Invalid position: $pos")
    }
    size * pos.y + pos.x
  }

  // TODO: Finish Impl
}
