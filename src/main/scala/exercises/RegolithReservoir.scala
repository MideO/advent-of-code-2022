package com.github.mideo
package exercises

// https://adventofcode.com/2022/day/14
case class Point(x: Int, y: Int) {
  def down: Point = copy(x, y + 1)

  def downLeft: Point = copy(x - 1, y + 1)

  def downRight: Point = copy(x + 1, y + 1)
}

object Point {
  def apply(input: String): Point = input match {
    case s"$x,$y" => Point(x.trim.toInt, y.trim.toInt)
  }
}

object RangeInclusive {
  def apply(from: Point, to: Point): Seq[Point] = {
    (from, to) match {
      case (Point(a, y), Point(b, _)) if a != b => numberRange(a, b) map (x => Point(x, y))
      case (Point(x, a), Point(_, b)) if a != b => numberRange(a, b) map (y => Point(x, y))
    }
  }
}

case class Rocks(rocks: Set[Point], floorDistance: Int = 0) {
  def contains(point: Point): Boolean = rocks.contains(point)

  lazy val LowestY: Int = rocks.max((x: Point, y: Point) => x.y.compareTo(y.y)).y
  lazy val Floor: Int = LowestY + floorDistance
}

object Rocks {
  private val points: String => Seq[Point] = line => line.split("->")
    .filter(_.nonEmpty)
    .sliding(2)
    .flatMap {
      x => RangeInclusive(Point(x.head.trim), Point(x.last.trim))
    }.toList

  def apply(input: String, floorDistance: Int): Rocks = {
    val rocks = iterator(input)
      .flatMap(points)
      .toSet

    Rocks(rocks, floorDistance)
  }
}


case class SandPile(pile: Set[Point])

object SandPile {
  private def next(point: Option[Point], rocks: Rocks, pile: Set[Point]): Option[(Point, Boolean)] = {
    val canMove: Point => Boolean = point => !rocks.contains(point) && !pile.contains(point)

    val isSettled: Point => Boolean = point => {
      val down = point.down
      val itemBelow = pile.contains(down) || rocks.contains(down) || down.y == rocks.Floor

      (itemBelow && !canMove(point.downLeft) && !canMove(point.downRight)) ||
        (down.y > rocks.LowestY && down.y == rocks.Floor)
    }

    val highest = point.filter(isSettled).filterNot(pile.contains)

    highest.orElse(point.map(_.down).filter(canMove))
      .orElse(point.map(_.downLeft).filter(canMove))
      .orElse(point.map(_.downRight).filter(canMove))
      .filter(_.y < rocks.Floor)
      .map(it => (it, isSettled(it)))

  }

  def drop(source: Point, rocks: Rocks, current: Point, accumulator: Set[Point] = Set.empty): Set[Point] =
    next(Option(current), rocks, accumulator) match {
      case Some((point, true)) =>
        drop(source, rocks, source, accumulator.incl(point))
      case Some((point, false)) =>
        drop(source, rocks, point, accumulator)
      case _ => accumulator
    }

  def apply(source: Point, rocks: Rocks, accumulator: Set[Point] = Set.empty): SandPile =
    SandPile(drop(source, rocks, source, accumulator))
}

object RegolithReservoir {
  def restingSandPile(input: String, source: Point = Point("500,0"), floorDistance: Int = 0): Int =
    SandPile(source, Rocks(input, floorDistance)).pile.size

}