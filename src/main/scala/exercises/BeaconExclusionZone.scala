package com.github.mideo
package exercises

// https://adventofcode.com/2022/day/15
case class Sensor(position: Position, beacon: Position) {
  val manhattanDistance: Int = (position.x - beacon.x).abs + (position.y - beacon.y).abs

  def range(yCoordinate: Int): Range = {
    val abs = manhattanDistance - (position.y - yCoordinate).abs
    position.x - abs to position.x + abs
  }
}

case class Grid(sensors: Seq[Sensor]) {
  val beacons: Seq[Position] = sensors.map(_.beacon).distinct
}

object Grid {
  def apply(input: String): Grid = {
    val sensors: Seq[Sensor] = iterator(input) map {
      case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" => Sensor(
        Position(x1.toInt, y1.toInt),
        Position(x2.toInt, y2.toInt)
      )
    } toSeq

    Grid(sensors)
  }
}


object BeaconExclusionZone {
  def positionThatCannotContainABeacon(input: String, yCoordinate: Int): Int = {
    val grid = Grid(input)
    val ranges = grid.sensors.flatMap(x => x.range(yCoordinate)).distinct
    val beacons = grid.beacons.filter(_.y == yCoordinate)
    ranges.size - beacons.size
  }

  def tuningFrequency(input: String, from: Int, to: Int): BigInt = {
    val grid = Grid(input)
    val target = from to to
    val unoccupied = target map {
      yCoordinate =>
        grid.sensors.map(x => x.range(yCoordinate))
          .filter(_.nonEmpty)
          .foldLeft(List(target)) {
            case (acc: List[Range], range: Range) => acc.flatMap {
              current =>
                val start = current.start to Math.min(range.start - 1, current.last)
                val end = Math.max(current.start, range.last + 1) to current.last
                List(start, end).filter(_.nonEmpty)
            }
          } flatMap { it => it.map(x => Position(x, yCoordinate)) }
    }

    val found = unoccupied.flatten.head
    BigInt(found.x) * 4_000_000 + found.y
  }
}


