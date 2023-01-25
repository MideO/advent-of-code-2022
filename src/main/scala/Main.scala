package com.github.mideo

import exercises._
import inputs._

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

  // Day 8
  // Q1
  print(
    "TreetopTreeHouse.visibleTreesFromBoarder",
    TreetopTreeHouse.visibleTreesFromBoarder(TreetopTreeHouseInput.value).length
  )
  // Q2
  print(
    "TreetopTreeHouse.highestScenicScore",
    TreetopTreeHouse.highestScenicScore(TreetopTreeHouseInput.value)
  )

  // Day 9
  // Q1
  print(
    "RopeBridge.processMovement, Two Knots",
    RopeBridge.processMovement(RopeBridgeInput.value, new Knot, new Knot).last.visited.size
  )

  // Q2
  print(
    "RopeBridge.processMovement, Ten Knots",
    RopeBridge.processMovement(
      RopeBridgeInput.value, (1 to 10).map(n => new Knot(n)):_*)(9).visited.size
  )

  // Day 10
  // Q1
  print(
    "CathodeRayTube.signalStrength, Cycle Stage During",
    CathodeRayTube.signalStrength(CathodeRayTubeInput.value, _.during, (20 to 220 by 40):_*)
  )
  // Q2
  print(
    "Screen::draw(CathodeRayTubeInput.value)",
    new Screen().draw(CathodeRayTubeInput.value)
  )

  // Day 11
  // Q1
  print(
    "MonkeyInTheMiddle.monkeyBusinessLevel after 20 rounds",
    MonkeyInTheMiddle.monkeyBusinessLevel(MonkeyInTheMiddle.play(MonkeyInTheMiddleInput.value, 20))
  )
  // Q2
  print(
    "MonkeyInTheMiddle.monkeyBusinessLevel after 10000 rounds no stress reducer",
    MonkeyInTheMiddle.monkeyBusinessLevel(MonkeyInTheMiddle.play(MonkeyInTheMiddleInput.value, 10000, 1))
  )

  // Day 12
  // Q1
  print(
    "HillClimbingAlgorithm.shortestRoute from S",
    HillClimbingAlgorithm.shortestRoute(Graph(HillClimbingAlgorithmInput.value), 'S')
  )
  // Q2
  print(
    "HillClimbingAlgorithm.shortestRoute from a",
    HillClimbingAlgorithm.shortestRoute(Graph(HillClimbingAlgorithmInput.value), 'a')
  )


  private def print[T](result:T*):Unit = result.foreach(println)
}
