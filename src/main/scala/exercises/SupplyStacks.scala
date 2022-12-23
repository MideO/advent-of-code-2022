package com.github.mideo
package exercises

import scala.collection.immutable.Map
import scala.collection.mutable.Stack
import scala.language.implicitConversions

// https://adventofcode.com/2022/day/5

case class StackItem(value: String)

case class SupplyStack(private[exercises] val items: Stack[StackItem] = new Stack[StackItem]()) {
  def top: StackItem = items.head
}


sealed trait Crane {
  private val get: (Map[Int, SupplyStack], Int) => SupplyStack = (stack, index) => stack(index)
  private val targets = Map(
    "from" -> get,
    "to" -> get
  )
  private val move: (Int, SupplyStack, SupplyStack) => Unit = (howMany, from, to) => {
    put(to, get(from, howMany): _*)
  }
  private val instruction: Map[String, (Int, SupplyStack, SupplyStack) => Unit] = Map(
    "move" -> move
  )

  def put(stack: SupplyStack, item: StackItem*): Unit

  def get(stack: SupplyStack, howMany: Int): Seq[StackItem] = (0 until howMany).map(_ => stack.items.removeHead(true))

  def move(input: String, stacks: Map[Int, SupplyStack]): Unit = {
    val split = input.split(" ")
    val howMany = split(1).toInt
    val from = targets(split(2))(stacks, split(3).toInt)
    val to = targets(split(4))(stacks, split(5).toInt)
    instruction(split(0))(howMany, from, to)
  }
}


object Stacks {
  private val BlankChar: Char = ' '

  implicit class CharOps(char: Char) {
    def nonBlank = char != BlankChar
  }

  implicit def stringConversion(char: Char): String = char.toString

  def apply(input: String): Map[Int, SupplyStack] = {
    val split = input.split("\n")
    val positions = split.last.toCharArray

    split.init.filter(_.nonEmpty)
      .flatMap(line => positions.filter(it => it.nonBlank)
        .map {
          position =>
            val index = positions.indexOf(position)
            val items = line.toCharArray
            (position, if (index < items.length) items(index) else BlankChar)
        }
        .filter(it => it._1.nonBlank && it._2.nonBlank)
        .map(it => (it._1.asDigit, StackItem(it._2)))).groupBy(_._1)
      .view
      .mapValues(v => {
        SupplyStack(Stack(v.map(it => it._2): _*))
      })
      .toMap
  }

}

object SingleStackCrateMover extends Crane {
  override def put(stack: SupplyStack, item: StackItem*): Unit = stack.items.pushAll(Stack(item: _*))

}

object MultiStackCrateMover extends Crane {
  override def put(stack: SupplyStack, item: StackItem*): Unit = stack.items.pushAll(item.reverse)
}

object SupplyStacks {
  def sort(crane: Crane, puzzle: String): Map[Int, SupplyStack] = {
    val stacksAndInstructions = puzzle.split("\n\n")
    val stacks: Map[Int, SupplyStack] = Stacks(stacksAndInstructions.head)

    stacksAndInstructions
      .last
      .split("\n")
      .foreach(input => crane.move(input, stacks))
    stacks
  }

  def topItems(puzzle: String)(implicit crane: Crane): String = sort(crane, puzzle)
    .toSeq
    .sortBy(_._1)
    .map(_._2.top.value)
    .mkString

}
