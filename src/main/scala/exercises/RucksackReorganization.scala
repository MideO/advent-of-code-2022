package com.github.mideo
package exercises


case class RucksackCompartment(items: String)

case class RuckSack(compartments: RucksackCompartment*) {
  def content: String = compartments.map(_.items).mkString

  def findCommon: Option[String] = compartments.map(_.items)
    .reduce(_.intersect(_))
    .headOption.map(_.toString)

}

object RuckSack {
  def apply(items: String): RuckSack = {
    val mid = items.length / 2
    RuckSack(RucksackCompartment(items.take(mid)), RucksackCompartment(items.drop(mid)))
  }
}

case class ElfGroup(elves: RuckSack*) {
  def findCommon: Option[String] = elves
    .map(_.content)
    .reduce(_.intersect(_))
    .headOption.map(_.toString)

}

object RucksackReorganization {
  def sumPriority(input: String ) = input
    .split("\n")
    .filter(_.nonEmpty)
    .map(it => RuckSack(it).findCommon.map(itemPriority).getOrElse(0))
    .sum

  def sumPriorityByElfGroup(input: String ) = {
    input
    .split("\n")
      .filter(_.nonEmpty)
    .sliding(3, 3)
    .map(it => it.map(i => RuckSack(i)))
    .map(ruckSacks => ElfGroup(ruckSacks:_*).findCommon.map(itemPriority).getOrElse(0))
    .sum
  }


  def itemPriority(str: String): Int = priorityOrder.indexOf(str) + 1

  private val priorityOrder = "abcdefghijklmnopqrstuvwxyzABCDEFGJIHKLMNOPQRSTUVWXYZ".toList.map(_.toString)

}
