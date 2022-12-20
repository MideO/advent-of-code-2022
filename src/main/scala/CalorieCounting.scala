package com.github.mideo

import scala.annotation.tailrec
import scala.collection.mutable.Stack

//https://adventofcode.com/2022/day/1
case class Elf(totalCalories: Long)

object Elf {
  def apply(stringOfNumbers: String): Elf = {

    val sum = stringOfNumbers.split("\n").foldLeft(0L) {
      case (total, "") => total
      case (total, x) => total + x.toLong
    }
    Elf(sum)
  }
}

object CalorieCounting {

  def getTopNElvesMostCalories(caloriesList: String, topNumberOfElves: Int): List[Elf] = sort(stack(caloriesList)).take(topNumberOfElves).toList


  def getElfMostCalories(caloriesList: String): Option[Elf] = {
    sort(stack(caloriesList)).headOption
  }

  private def stack(caloriesList: String): Stack[Elf] = {
    Stack.from(caloriesList.split("\n\n")
      .filter(it => it.nonEmpty)
      .map(Elf.apply))
  }

  @tailrec
  private def sort(stack: Stack[Elf], accumulator: Stack[Elf] = Stack.empty[Elf]): Stack[Elf] = {
    @tailrec
    def innerSort(stackElf: Elf, sorted: Stack[Elf], acc: Stack[Elf]): Stack[Elf] = {
      if (sorted.isEmpty || stackElf.totalCalories >= sorted.head.totalCalories) Stack.from(sorted).prepend(stackElf).prependAll(acc)
      else innerSort(stackElf, sorted.tail, Stack.from(acc).append(sorted.head))
    }

    if (stack.isEmpty) accumulator
    else sort(stack.tail, innerSort(stack.head, accumulator, Stack.empty[Elf]))

  }
}
