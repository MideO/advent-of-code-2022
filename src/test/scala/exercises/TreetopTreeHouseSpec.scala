package com.github.mideo
package exercises

class TreetopTreeHouseSpec extends TestSpec {
  val input =
    """
      |30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin

  "Quadcopter" should {
    "build map of trees with single row" in {
      Quadcopter.scan("30373") should be(
        MapOfTrees(Seq(
          Seq(Tree(3, 0, 0), Tree(0, 0, 1), Tree(3, 0, 2), Tree(7, 0, 3), Tree(3, 0, 4)),
        ))
      )
    }
    "build map of trees with multiple row" in {
      Quadcopter.scan(input) should be(
        MapOfTrees(Seq(
          Seq(Tree(3, 0, 0), Tree(0, 0, 1), Tree(3, 0, 2), Tree(7, 0, 3), Tree(3, 0, 4)),
          Seq(Tree(2, 1, 0), Tree(5, 1, 1), Tree(5, 1, 2), Tree(1, 1, 3), Tree(2, 1, 4)),
          Seq(Tree(6, 2, 0), Tree(5, 2, 1), Tree(3, 2, 2), Tree(3, 2, 3), Tree(2, 2, 4)),
          Seq(Tree(3, 3, 0), Tree(3, 3, 1), Tree(5, 3, 2), Tree(4, 3, 3), Tree(9, 3, 4)),
          Seq(Tree(3, 4, 0), Tree(5, 4, 1), Tree(3, 4, 2), Tree(9, 4, 3), Tree(0, 4, 4))
        ))
      )
    }}

  "TreetopTreeHouse" should{

    "count visible trees" in {
      val visible = TreetopTreeHouse.visibleTreesFromBoarder(input)
      visible.length should be(21)
    }

    "find highest scenic score" in {
      TreetopTreeHouse.highestScenicScore(input) should be((Tree(5, 3, 2), 8))
    }
  }

}
