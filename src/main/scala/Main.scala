package com.github.mideo

import inputs._

import com.github.mideo.exercises._

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


  // Day 4
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

  // Day 5
  // Q1
  print(
    "SupplyStacks.topItems:: SingleStackCrateMover",
    SupplyStacks.topItems(SupplyStacksInput.value)(SingleStackCrateMover)
  )
  // Q2
  print(
    "SupplyStacks.topItems:: MultiStackCrateMover",
    SupplyStacks.topItems(SupplyStacksInput.value)(MultiStackCrateMover)
  )

  // Day 6
  // Q1
  print(
    "TuningTrouble.messageBeginPosition:: StartOfPacketMarker",
    TuningTrouble.startMarkerIndex(TuningTroubleInput.value, StartOfPacketMarker)
  )

  // Q2
  print(
    "TuningTrouble.messageBeginPosition:: StartOfMessageMarker",
    TuningTrouble.startMarkerIndex(TuningTroubleInput.value, StartOfMessageMarker)
  )


  // Day 7
  // Q1
  print(
    "NoSpaceLeftOnDevice.sum :: `d => d.size < 100000` ",
    NoSpaceLeftOnDevice.sum(NoSpaceLeftOnDeviceInput.value, d => d.size < 100000)
  )

  // Q2
  print(
    "NoSpaceLeftOnDevice.findSmallestDirectorySizeToDeleteToPerformUpdate",
    NoSpaceLeftOnDevice.findSmallestDirectorySizeToDeleteToPerformUpdate(NoSpaceLeftOnDeviceInput.value, 30000000L,70000000L)
  )



  private def print[T](result:T*):Unit = result.foreach(println)
}
