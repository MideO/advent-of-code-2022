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



  private def print[T](result:T*):Unit = result.foreach(println)
}
