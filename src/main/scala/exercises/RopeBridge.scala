package com.github.mideo
package exercises


import exercises.Direction.Direction

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/9
object Direction extends Enumeration {
  type Direction = Value
  val R, L, U, D = Value
}



class Knot(number: Int = 1, private val StartingPosition:Position = Position(0, 0)) {

  private val Step: Int = 1
  override def toString: String = s"Knot($number)"

  private val trail: Stack[Position] = Stack(StartingPosition)
  def visited: Stack[Position] = trail.distinct

  def position: Position = trail.head

  def move(direction: Direction): Unit = direction match {
    case Direction.R => move(position.x + Step, position.y)
    case Direction.L => move(position.x - Step, position.y)
    case Direction.U => move(position.x, position.y + Step)
    case Direction.D => move(position.x, position.y - Step)
  }

  def moveCloser(head: Knot): Unit = {
    val result = (head.position.x - position.x, head.position.y - position.y) match {
      case (x, _) if x > Step =>
        Position(position.x + Step, head.position.y)
      case (x, _) if x < -Step =>
        Position(position.x - Step, head.position.y)
      case (_, y) if y > Step =>
        Position(head.position.x, position.y + Step)
      case (_, y) if y < -Step  =>
        Position(head.position.x, position.y - Step)
      case _ => position
    }
    move(result.x, result.y)
  }

  def move(x: Int, y: Int): Unit = {
    trail.push( Position(x, y))
  }
}

object Repeat {
  def apply(times: Int)(action: Int => Unit): Unit = (1 to times).foreach(action)
}

object RopeBridge {
  def processMovement(input: String, knots: Knot*): Seq[Knot] = {
    iterator(input)
      .foreach {
        line =>
          val split = line.split(" ")
          Repeat(split.last.toInt) {
            _ =>
              knots.head.move(Direction.withName(split.head))
              knots.sliding(2).foreach(pair => pair.last.moveCloser(pair.head))
          }

      }
    knots
  }

}
