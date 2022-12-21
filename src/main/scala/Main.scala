package com.github.mideo

import inputs._

import com.github.mideo.exercises.{CalorieCounting, CampCleanup, PlayWithGameOptionStrategy, PlayWithOutcomeStrategy, RockPaperScissors, RucksackReorganization}

object Main extends App {
  // Day 1
  // Q1
  print (
    "CalorieCounting.getElfMostCalories:",
    CalorieCounting.getElfMostCalories(CalorieCountingInput.value)
  )
  // Q2
  print(
    "CalorieCounting.getTopNElvesMostCalories",
    CalorieCounting.getTopNElvesMostCalories(CalorieCountingInput.value, 3).map(_.totalCalories).sum
  )


  // Day 2
  // Q1
  print(
    "RockPaperScissors.playWithStrategy PlayWithGameOptionStrategy",
    RockPaperScissors.playWithStrategy(RockPaperScissorInput.value, PlayWithGameOptionStrategy)
  )

  // Q2
  print(
    "RockPaperScissors.playWithStrategy PlayWithOutcomeStrategy",
    RockPaperScissors.playWithStrategy(RockPaperScissorInput.value, PlayWithOutcomeStrategy)
  )

  // Day 3
  // Q1
  print(
    "RucksackReorganization.sumPriority",
    RucksackReorganization.sumPriority(RucksackReorganizationInput.value)
  )
  // Q2
  print(
    "RucksackReorganization.sumPriorityByElfGroup",
    RucksackReorganization.sumPriorityByElfGroup(RucksackReorganizationInput.value)
  )


  // Day4
  // Q1
  print(
    "CampCleanup.countPairs pair.isFullyContained",
    CampCleanup.countPairs(CampCleanupInput.value, pair => pair.isFullyContained)
  )
  // Q2
  print(
    "CampCleanup.countPairs pair.isOverlapping",
    CampCleanup.countPairs(CampCleanupInput.value, pair => pair.isOverlapping)
  )


  private def print[T](result:T*):Unit = result.foreach(println)
}
