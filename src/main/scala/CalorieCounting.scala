package com.github.mideo

//https://adventofcode.com/2022/day/1
object CalorieCounting {

  def getElfMostCalories(caloriesList: String): Option[Elf] = {
    caloriesList.split("\n\n")
      .filter(it => it.nonEmpty)
      .zipWithIndex
      .map(it => {
        val sum  = it._1.split("\n").foldLeft(0L) {
          case (total, "") => total
          case (total, x) => total + x.toLong
        }
        Elf(it._2+1, sum)
      }).maxOption((x: Elf, y: Elf) => x.totalCalories.compareTo(y.totalCalories))

  }

  case class Elf(number: Int, totalCalories: Long)

}
