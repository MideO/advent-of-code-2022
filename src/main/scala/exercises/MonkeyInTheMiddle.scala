package com.github.mideo
package exercises


import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.{Stack, Map => MutableMap}
// https://adventofcode.com/2022/day/11
case class Item(value: Long) {
  def isDivisibleBy(x: Int): Boolean = value % x == 0
}

class ItemInspectionCounter {
  private val counter: AtomicInteger = new AtomicInteger(0)

  def inc: Unit = counter.incrementAndGet()

  def get: Long = counter.get()
}

class Collection[T](default: T*) {
  private val stack: Stack[T] = Stack(default: _*)

  def nonEmpty: Boolean = stack.nonEmpty

  def isEmpty: Boolean = stack.isEmpty

  def add(item: T): Unit = stack.append(item)

  def head: T = stack.pop()

  def equals(that: Collection[T]): Boolean = stack.equals(that.stack)

}

trait ThrowAndReceiveItem[T] {
  val Items: Collection[T]

  def throwItem(item: T, that: ThrowAndReceiveItem[T]): Unit = that.receiveItem(item)

  def all: Collection[T] = Items

  private def receiveItem(item: T): Unit = Items.add(item)
}

class Monkeys {
  private val map = MutableMap.empty[Int, Monkey]

  def map[T](mapper: Monkey => T): Map[Int, T] = (0 until (map.size))
    .filter(map.contains)
    .map(key => (key, mapper(map(key))))
    .toMap

  def add(id: Int, monkey: Monkey): Unit = map(id) = monkey

  def get(id: Int): Monkey = map(id)
}

object Monkeys {
  def apply(input: String, reducer: Int): Monkeys = {
    val monkeys = new Monkeys
    input.split("\n\n")
      .foreach(block => Monkey(block, monkeys, reducer))
    monkeys
  }
}

class Monkey(monkeys: Monkeys, options: (Int, Int), operation: String,
             incrementer: String, reducer: Int, val divisor: Int, items: Item*)
  extends ThrowAndReceiveItem[Item] {
  private val _options = options

  private def calculate(item: Item): Item = {
    val other = if (incrementer.equals("old")) item.value else incrementer.toLong
    if (operation.equals("*")) Item(((item.value * other) / reducer) % monkeys.map(_.divisor).values.product)
    else Item((item.value + other) / reducer)
  }

  def throwItems(): Unit = {
    while (all.nonEmpty) {
      val result = calculate(all.head)
      val target = {
        if (result.isDivisibleBy(divisor)) monkeys.get(_options._1)
        else monkeys.get(_options._2)
      }
      throwItem(result, target)
      InspectionCounter.inc
    }

  }

  val InspectionCounter = new ItemInspectionCounter
  override val Items: Collection[Item] = new Collection(items: _*)

  def equals(that: Monkey): Boolean =
    Items.equals(that.Items) &&
      _options.equals(that._options) &&
      calculate(Item(10)).equals(that.calculate(Item(10)))
}

object Monkey {
  def apply(input: String, monkeys: Monkeys, reducer: Int): Monkey = {
    val split = input.split("\n")
      .filter(_.nonEmpty)

    val id: Int = split(0).init.last.asDigit
    val items: Seq[Item] = split(1)
      .split(":")
      .last.split(",")
      .toSeq
      .map(it => Item(it.trim.toInt))

    val ins = split(2)
      .replace("Operation: new = ", "").trim
      .split(" ")

    val divisor = split(3).split(" ").last.toInt
    val options = (split(4).split(" ").last.toInt, split(5).split(" ").last.toInt)

    val monkey = new Monkey(monkeys, options, ins(1), ins.last, reducer, divisor, items: _*)
    monkeys.add(id, monkey)
    monkey
  }
}

object MonkeyInTheMiddle {
  def play(input: String, numberOfRounds: Int, reducer: Int = 3): Monkeys = {
    val monkeys = Monkeys(input, reducer)
    (1 to numberOfRounds) foreach (_ => monkeys.map(_.throwItems()))
    monkeys
  }

  def monkeyBusinessLevel(monkeys: Monkeys): Long = {
    monkeys
      .map(it => it.InspectionCounter.get)
      .values
      .toSeq
      .sorted
      .takeRight(2)
      .product
  }
}
