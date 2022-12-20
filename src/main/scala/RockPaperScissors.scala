package com.github.mideo

import GamePoint.{Draw, Loss, Win}
import UserInput.{X, Y, Z}

//https://adventofcode.com/2022/day/2
private object GamePoint {
  val Win = 6
  val Draw = 3
  val Loss = 0
}

private object UserInput {
  val X = "X"
  val Y = "Y"
  val Z = "Z"
}

sealed trait GameChoice {
  val score: Int
  val lowerGameChoice: GameChoice
  val higherGameChoice: GameChoice
}

object Rock extends GameChoice {
  override val score: Int = 1
  override val lowerGameChoice: GameChoice = Scissors
  override val higherGameChoice: GameChoice = Paper
}

object Paper extends GameChoice {
  override val score: Int = 2
  override val lowerGameChoice: GameChoice = Rock
  override val higherGameChoice: GameChoice = Scissors
}

object Scissors extends GameChoice {
  override val score: Int = 3
  override val lowerGameChoice: GameChoice = Paper
  override val higherGameChoice: GameChoice = Rock
}


trait RockPaperScissorsStrategy[T] {
  val mapping: Map[String, T]

  def execute(opponentInput: GameChoice, input: String): Int
}

object PlayWithGameOptionStrategy extends RockPaperScissorsStrategy[GameChoice] {
  val mapping: Map[String, GameChoice] = Map(X -> Rock, Y -> Paper, Z -> Scissors)
  override def execute(opponentInput: GameChoice, input: String): Int = mapping(input) match {
      case x if x.lowerGameChoice.equals(opponentInput) => Win + x.score
      case x if x.equals(opponentInput) => Draw + x.score
      case x => Loss + x.score
    }
}

object PlayWithOutcomeStrategy extends RockPaperScissorsStrategy[Int] {
  override val mapping: Map[String, Int] = Map(X -> Loss, Y -> Draw, Z -> Win)
  override def execute(opponentInput: GameChoice, input: String): Int = mapping(input) match {
      case x if x == Win => opponentInput.higherGameChoice.score + x
      case x if x == Draw => opponentInput.score + x
      case x  => opponentInput.lowerGameChoice.score + x
    }
}

object RockPaperScissors {
  private val InputMapping: Map[String, GameChoice] = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)
  def playWithStrategy[T](input: String, strategy: RockPaperScissorsStrategy[T]): Int = {
    input.split("\n")
      .filter(_.nonEmpty)
      .map { it =>
        val split = it.split(" ")
        strategy.execute(InputMapping(split.head), split.last)
      }.sum
  }
}