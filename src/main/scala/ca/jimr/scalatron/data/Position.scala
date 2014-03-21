package ca.jimr.scalatron.data

/*
 * Generic (x,y) position.
 */

case class Position(x: Int, y: Int) {
  override def toString = s"$x:$y"

  def +(other: Position) = {
    copy(x = x + other.x, y = y + other.y)
  }

  def -(other: Position) = {
    copy(x = x - other.y, y = y - other.y)
  }

  def length = math.sqrt(x * x + y * y)

  // TODO: Finish impl
}

object Position {
  def apply(input: String): Position = {
    val parts = input.split(':')
    Position(parts(0).toInt, parts(1).toInt)
  }
}
