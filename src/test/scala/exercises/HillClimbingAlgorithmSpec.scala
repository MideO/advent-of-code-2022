package com.github.mideo
package exercises


class HillClimbingAlgorithmSpec extends TestSpec {

  "Climber" should {
    "move" in {
      Climber(Node(1, 2)).move(1, 2) should be(Climber(Node(2, 4)))
    }

    "return possible moves" in {
      Climber(Node(0, 0)).options(Map(
        Node(0, 0) -> 'A',
        Node(0, 1) -> 'B',
        Node(1, 0) -> 'C',
        Node(1, 1) -> 'C'
      )) should contain theSameElementsAs Seq(Node(0, 1), Node(1, 0))

    }
  }
  "Graph" should {
    val input =
      """
        |Sabqponm
        |abcryxxl
        |accszExk
        |acctuvwj
        |abdefghi
        |""".stripMargin
    val graph = Graph(input)

    "apply function" in {
      graph(_.size) should be(40)
    }
    "build graph" in {

      graph(_(Node(0, 0))) should be('S')
      graph(_(Node(5, 2))) should be('E')
    }

    "findFirst Char" in {
      graph.findLast('S') should be(Node(0, 0))
      graph.findLast('E') should be(Node(5, 2))

    }
  }
  "HillClimbingAlgorithm" should {
    val input =
      """
        |Sabqponm
        |abcryxxl
        |accszExk
        |acctuvwj
        |abdefghi
        |""".stripMargin
    val graph = Graph(input)
    "count shortest path from S" in {
      HillClimbingAlgorithm.shortestRoute(graph, 'S') should be (31)
    }
    "count shortest path from a" in {
      HillClimbingAlgorithm.shortestRoute(graph, 'a') should be(29)
    }
  }
}

