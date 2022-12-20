package com.github.mideo

object Main extends App {
  // Day 1
  // Q1
  print (
    "CalorieCounting.getElfMostCalories:",
    CalorieCounting.getElfMostCalories(Inputs.CalorieCountingInput)
  )
  // Q2
  print(
    "CalorieCounting.getTopNElvesMostCalories",
    CalorieCounting.getTopNElvesMostCalories(Inputs.CalorieCountingInput, 3).map(_.totalCalories).sum
  )


  // Day 2
  // Q1
  print(
    "RockPaperScissors.playWithStrategy PlayWithGameOptionStrategy",
    RockPaperScissors.playWithStrategy(Inputs.RockPaperScissorsStrategy, PlayWithGameOptionStrategy)
  )

  // Q2
  print(
    "RockPaperScissors.playWithStrategy PlayWithOutcomeStrategy",
    RockPaperScissors.playWithStrategy(Inputs.RockPaperScissorsStrategy, PlayWithOutcomeStrategy)
  )

  // Day 3
  // Q1
  print(
    "RucksackReorganization.sumPriority",
    RucksackReorganization.sumPriority(Inputs.RucksackReorganizationInput)
  )
  // Q2
  print(
    "RucksackReorganization.sumPriorityByElfGroup",
    RucksackReorganization.sumPriorityByElfGroup(Inputs.RucksackReorganizationInput)
  )
  private def print[T](result:T*):Unit = result.foreach(println)
}
