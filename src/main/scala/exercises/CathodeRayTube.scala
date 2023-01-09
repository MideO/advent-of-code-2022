package com.github.mideo
package exercises

import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/10
sealed trait Instruction {
  def cycles: Seq[Cycle]
}

case class Cycle(before: Register, during: Register, after: Register) {
  def sequence: Seq[Register] = Seq(before, during, after).distinct
}

case class NoOp(register: Register) extends Instruction {

  override def cycles: Seq[Cycle] = Seq(Cycle(register, register, register))
}

case class AddX(register: Register, x: Int) extends Instruction {
  override def cycles: Seq[Cycle] = Seq(Cycle(register, register, register), Cycle(register, register, register + x))
}


case class Register(value: Int = 1) {
  def `+`(x: Int): Register = this.copy(value + x)

  def `*`(x: Int): Int = value * x
}

object Cpu {
  def processInput(input: String): Map[Int, Cycle] = {
    var register = Register()
    input
      .split("\n")
      .filter(_.nonEmpty)
      .flatMap(
        _.split(" ")
          .toList match {
          case _ :: xs =>
            val result = xs.headOption
              .map(it => AddX(register, it.toInt).cycles)
              .getOrElse(NoOp(register).cycles)
            register = result.last.after
            result
          case _ => NoOp(register).cycles
        }
      ).zipWithIndex
      .map(it => it._2 + 1 -> it._1)
      .toMap

  }
}


object CathodeRayTube {
  def signalStrength(input: String, stage: Cycle => Register, cycles: Int*): Int = {
    val processed = Cpu.processInput(input)
    cycles.map(each => stage(processed(each)) * each).sum
  }
}


case class Sprite(middleIndex: Int) {
  def coveredIndices(index: Int): Boolean =
    Seq(middleIndex - 1, middleIndex, middleIndex + 1).contains(index)


}

class Screen {
  private val dimensionRange: Range.Inclusive = 1 to 240
  private val width = 40
  private val display = ListBuffer[String](dimensionRange.map(_ => "."): _*)

  def drawPixel(character: String, positions: Int*): Unit = positions
    .foreach(position => display(position) = character)

  def render: String = display.grouped(width)
    .map(_.mkString)
    .mkString("\n")

  private def processInput(input: String): Array[Register] = {
    var register = Register()
    for {
     line <- input.split("\n").filter(_.nonEmpty)
      sequence <- {
        val split = line.split(" ")
        val result = {
          if (split.length == 2) AddX(register, split.last.toInt).cycles.last
          else NoOp(register).cycles.last
        }
        register = result.after
        result.sequence
      }
    } yield sequence

  }

  def draw(input: String): String = {
    val instructions = processInput(input)
    var segment = 0
    var sprite = Sprite(1)
    display
      .zipWithIndex
      .foreach(
        pair => {
          val head = pair._2 - segment
          if (sprite.coveredIndices(head)) drawPixel("#", pair._2)
          if (head + 1 == width) segment += width
          sprite = Sprite(instructions(pair._2).value)
        }
      )
    render
  }
}


