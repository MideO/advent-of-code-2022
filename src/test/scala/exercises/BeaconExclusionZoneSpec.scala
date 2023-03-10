package com.github.mideo
package exercises

class BeaconExclusionZoneSpec extends TestSpec {
  "Sensor" should {
    "have beaconDistance" in {
      Sensor(Position(8, 7), Position(2, 10)).manhattanDistance should be(9)
    }
    "return span of sensor" in {
      Sensor(Position(8, 7), Position(2, 10)).range(7).size should be(19)
    }

  }
  "Grid" should {
    "load sensors" in {
      val input =
        """
          |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
          |Sensor at x=9, y=16: closest beacon is at x=10, y=16
          |Sensor at x=13, y=2: closest beacon is at x=15, y=3
          |Sensor at x=12, y=14: closest beacon is at x=10, y=16
          |""".stripMargin

      Grid(input) should be(
        Grid(
          Seq(
            Sensor(Position(2, 18), Position(-2, 15)),
            Sensor(Position(9, 16), Position(10, 16)),
            Sensor(Position(13, 2), Position(15, 3)),
            Sensor(Position(12, 14), Position(10, 16))
          )
        )
      )
    }


    "return all beacon positions" in {
      val input =
        """
          |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
          |Sensor at x=9, y=16: closest beacon is at x=10, y=16
          |Sensor at x=13, y=2: closest beacon is at x=15, y=3
          |Sensor at x=12, y=14: closest beacon is at x=10, y=16
          |Sensor at x=10, y=20: closest beacon is at x=10, y=16
          |Sensor at x=14, y=17: closest beacon is at x=10, y=16
          |Sensor at x=8, y=7: closest beacon is at x=2, y=10
          |Sensor at x=2, y=0: closest beacon is at x=2, y=10
          |Sensor at x=0, y=11: closest beacon is at x=2, y=10
          |Sensor at x=20, y=14: closest beacon is at x=25, y=17
          |Sensor at x=17, y=20: closest beacon is at x=21, y=22
          |Sensor at x=16, y=7: closest beacon is at x=15, y=3
          |Sensor at x=14, y=3: closest beacon is at x=15, y=3
          |Sensor at x=20, y=1: closest beacon is at x=15, y=3
          |""".stripMargin

      Grid(input).beacons should contain theSameElementsAs Seq(
        Position(-2, 15),
        Position(15, 3),
        Position(10, 16),
        Position(2, 10),
        Position(25, 17),
        Position(21, 22)
      )
    }
  }

  "BeaconExclusionZone" should {
    val input =
      """
        |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        |Sensor at x=9, y=16: closest beacon is at x=10, y=16
        |Sensor at x=13, y=2: closest beacon is at x=15, y=3
        |Sensor at x=12, y=14: closest beacon is at x=10, y=16
        |Sensor at x=10, y=20: closest beacon is at x=10, y=16
        |Sensor at x=14, y=17: closest beacon is at x=10, y=16
        |Sensor at x=8, y=7: closest beacon is at x=2, y=10
        |Sensor at x=2, y=0: closest beacon is at x=2, y=10
        |Sensor at x=0, y=11: closest beacon is at x=2, y=10
        |Sensor at x=20, y=14: closest beacon is at x=25, y=17
        |Sensor at x=17, y=20: closest beacon is at x=21, y=22
        |Sensor at x=16, y=7: closest beacon is at x=15, y=3
        |Sensor at x=14, y=3: closest beacon is at x=15, y=3
        |Sensor at x=20, y=1: closest beacon is at x=15, y=3
        |""".stripMargin

    "return positionThatCannotContainABeacon" in {
      BeaconExclusionZone.positionThatCannotContainABeacon(input, 10) should be(26)
    }

    "return tuningFrequency" in {
      BeaconExclusionZone.tuningFrequency(input, 0, 20) should be(56000011)
    }
  }
}


