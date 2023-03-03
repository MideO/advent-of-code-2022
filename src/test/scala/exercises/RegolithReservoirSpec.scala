package com.github.mideo
package exercises

class RegolithReservoirSpec extends TestSpec {
  "Point" should {
    "create point from string" in {
      Point(" 498 , 4") should be(Point(498, 4))
    }
  }

  "RangeInclusive" should {
    "create incremental inclusive range of points on x axis between two points" in {
      RangeInclusive(Point(498, 4), Point(500, 4)) should contain theSameElementsInOrderAs
        Seq(
          Point(498, 4),
          Point(499, 4),
          Point(500, 4)
        )
    }

    "create decremental inclusive range of points on x axis between two points" in {
      RangeInclusive(Point(500, 4), Point(498, 4)) should contain theSameElementsInOrderAs
        Seq(
          Point(500, 4),
          Point(499, 4),
          Point(498, 4)
        )

    }

    "create incremental points on y axis between two points" in {
      RangeInclusive(Point(500, 4), Point(500, 10)) should contain theSameElementsInOrderAs
        Seq(
          Point(500, 4),
          Point(500, 5),
          Point(500, 6),
          Point(500, 7),
          Point(500, 8),
          Point(500, 9),
          Point(500, 10)

        )
    }

    "create decremental points on y axis between two points" in {
      RangeInclusive(Point(500, 10), Point(500, 4)) should contain theSameElementsInOrderAs
        Seq(
          Point(500, 10),
          Point(500, 9),
          Point(500, 8),
          Point(500, 7),
          Point(500, 6),
          Point(500, 5),
          Point(500, 4)

        )

    }
  }


  "Rocks" should {
    val input =
      """
        |498,4 -> 498,6 -> 496,6
        |503,4 -> 502,4 -> 502,9 -> 494,9
        |""".stripMargin
    val rocks = Rocks(input, 0)

    "load rocks from input" in {
      rocks should be(Rocks(
        Set(
          Point(498, 4),
          Point(498, 5),
          Point(498, 6),
          Point(497, 6),
          Point(496, 6),
          Point(503, 4),
          Point(502, 4),
          Point(502, 5),
          Point(502, 6),
          Point(502, 7),
          Point(502, 8),
          Point(502, 9),
          Point(501, 9),
          Point(500, 9),
          Point(499, 9),
          Point(498, 9),
          Point(497, 9),
          Point(496, 9),
          Point(495, 9),
          Point(494, 9))
      )
      )
    }

    "return true if rock contains point" in {
      rocks.contains(Point(500, 9)) should be(true)
    }
    "return false if rock contains point" in {
      rocks.contains(Point(555, 1)) should be(false)
    }

    "return lowest rock" in {
      rocks.LowestY should be (9)
    }


    "return floor when distance is 0" in {
      rocks.Floor should be(9)
    }

    "return floor when distance is non 0" in {
      Rocks(input, 2).Floor should be(11)
    }




  }


  "SandPile" should {
    "return a set of covered points fro simple rocks pile" in {
      val input =
        """
          |495,5 -> 496,5 -> 497,5 -> 498,5
          |""".stripMargin
      SandPile(Point("497, 1"), Rocks(input, 0)) should be(
        SandPile(Set(Point(497, 4), Point(496, 4)))
      )
    }
  }
  "RegolithReservoir" should {
    "return a set of covered points when no floor" in {
      val input =
        """
          |498,4 -> 498,6 -> 496,6
          |503,4 -> 502,4 -> 502,9 -> 494,9
          |""".stripMargin

      RegolithReservoir.restingSandPile(input, Point("500,0")) should be(24)

    }
    "return a set of covered points when floor is set" in {
      val input =
        """
          |498,4 -> 498,6 -> 496,6
          |503,4 -> 502,4 -> 502,9 -> 494,9
          |""".stripMargin

      RegolithReservoir.restingSandPile(input,  Point("500,0"), 2) should be(93)

    }
  }

}
