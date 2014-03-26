package ca.jimr.scalatron.api

/*
 * Generic (x,y) position.
 */

case class Position(x: Int, y: Int) {
  override def toString = x+":"+y

  def +(other: Position) = {
    copy(x = x + other.x, y = y + other.y)
  }

  def -(other: Position) = {
    copy(x = x - other.y, y = y - other.y)
  }

  def length = math.sqrt(x * x + y * y).toInt
  def steps = math.abs(x) + math.abs(y)

  // TODO: Finish impl
}

object Position {
  def apply(input: String): Position = {
    val parts = input.split(':')
    Position(parts(0).toInt, parts(1).toInt)
  }
}
