package com.github.mideo
package exercises

// https://adventofcode.com/2022/day/4
case class Section(block:String){
  def sequence:Seq[Int] = {
    val range = block.split("-")
    range.head.toInt to range.last.toInt
  }
}
case class ElfPair(first:Section, second:Section) {
  def isFullyContained:Boolean =
    first.sequence.containsSlice(second.sequence) ||
    second.sequence.containsSlice(first.sequence)

  def isOverlapping: Boolean =
    first.sequence.exists(it => second.sequence.contains(it)) ||
      second.sequence.exists(it => first.sequence.contains(it))

}

object CampCleanup {
  def countPairs(input: String, criteria: ElfPair => Boolean):Int = {
    input.split("\n")
      .filter(_.nonEmpty)
      .map(it => {
        val pair = it.split(",")
        ElfPair(Section(pair.head), Section(pair.last))
      }).count(criteria.apply)
  }

}
