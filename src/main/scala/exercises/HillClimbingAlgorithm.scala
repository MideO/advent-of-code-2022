package com.github.mideo
package exercises

import exercises.Climber.moves

import scala.annotation.tailrec
import scala.collection.mutable

//https://adventofcode.com/2022/day/12
case class Node(x: Int, y: Int)

object Climber {
  val moves: Seq[(Int, Int)] = Seq((0, 1), (0, -1), (-1, 0), (1, 0))
}

case class Climber(node: Node) {
  def move(byX: Int, byY: Int): Climber = Climber(node.copy(node.x + byX, node.y + byY))

  def options(graph: Map[Node, Char]): Seq[Node] = moves
    .map(x => move(x._1, x._2))
    .filter(climber => graph.contains(climber.node))
    .map(_.node)
}

case class Graph(input: String) {
  private lazy val graph: Map[Node, Char] = {
    val split = IndexedSeq.from(input.linesIterator.filter(_.nonEmpty))
    split.indices
      .flatMap {
        y =>
          split.head
            .indices
            .map(x => Node(x, y) -> split(y)(x))
      }.toMap
  }
  def apply[T](func: Map[Node, Char] => T): T = func(graph)
  def findLast(char: Char): Node = this (_.map(_.swap))(char)
  def normalize(node: Node): Char = this {
    _(node) match {
      case 'S' => 'a'
      case 'E' => 'z'
      case x => x
    }
  }
}

object HillClimbingAlgorithm {
  def shortestRoute(graph: Graph, startChar: Char): Int = {
    val end = graph.findLast('E')
    routes(graph, startChar, mutable.Stack(end), mutable.Map(end -> 0))
  }
  @tailrec
  private def routes(graph: Graph, start: Char, stack: mutable.Stack[Node], paths: mutable.Map[Node, Int]): Int = {
    val visited = stack.pop()
    if (graph(_(visited)) == start) return paths(visited)

    graph(Climber(visited).options)
      .filter(option => !paths.contains(option) && graph.normalize(visited) - graph.normalize(option) <= 1)
      .foreach { option =>
        stack.append(option)
        paths(option) = paths(visited) + 1
      }

    routes(graph, start, stack, paths)
  }

}
