package com.github.mideo
package exercises

class CathodeRayTubeSpec extends TestSpec {

  "Register" should {
    "add increment" in {
      Register() + 1 should be(Register(2))
    }
    "add negative increment" in {
      Register(2) + (-10) should be(Register(-8))
    }
    "multiply increment" in {
      Register(10) * 10 should be(100)
    }

  }

  "Instruction" should {
    "apply NoOp" in {
      NoOp(Register()).cycles should be(Seq(Cycle(Register(1), Register(1), Register(1))))
    }

    "apply AddX" in {
      AddX(Register(), 10).cycles should be(Seq(Cycle(Register(1), Register(1), Register(1)), Cycle(Register(1), Register(1), Register(11))))
    }
  }

  "Cpu" should {
    "process input" in {
      val input =
        """
          |noop
          |addx 3
          |addx -5
          |""".stripMargin
      val result = Cpu.processInput(input)
      result should be(
        Map(
          1 -> Cycle(Register(1), Register(1), Register(1)),
          2 -> Cycle(Register(1), Register(1), Register(1)),
          3 -> Cycle(Register(1), Register(1), Register(4)),
          4 -> Cycle(Register(4), Register(4), Register(4)),
          5 -> Cycle(Register(4), Register(4), Register(-1))
        )
      )
    }
  }


  "CathodeRayTube" should {
    "calculate signal strength" in {
      val input =
        """
          |noop
          |addx 3
          |addx -5
          |""".stripMargin
      CathodeRayTube.signalStrength(input, _.during, 4) should be(16)
    }

    "calculate signal strength for bigger input" in {
      CathodeRayTube.signalStrength(bigInput, _.during, (20 to 220 by 40): _*) should be(13140)
    }
  }
  "Sprite" should {
    "be true when index is covered" in  {
      val sprite = Sprite(1)
      sprite.coveredIndices(0) should be(true)
      sprite.coveredIndices(1) should be(true)
      sprite.coveredIndices(2) should be(true)

    }
    "be false when index is not covered" in  {
      val sprite  = Sprite(2)
      sprite.coveredIndices(0) should be(false)
      sprite.coveredIndices(4) should be(false)

    }
  }
  "Screen" should {
    "render display" in {
      val screen = new Screen
      screen.render should be(
        """........................................
          |........................................
          |........................................
          |........................................
          |........................................
          |........................................""".stripMargin
      )
    }

    "render display after" in {
      val screen = new Screen
      screen.drawPixel("#", 0, 1, 2, 3, 40, 80, 81, 82, 120, 160, 200)
      screen.render should be(
        """####....................................
          |#.......................................
          |###.....................................
          |#.......................................
          |#.......................................
          |#.......................................""".stripMargin
      )
    }
    "draw CRT input" in {

      val expected =
        """##..##..##..##..##..##..##..##..##..##..
          |###...###...###...###...###...###...###.
          |####....####....####....####....####....
          |#####.....#####.....#####.....#####.....
          |######......######......######......####
          |#######.......#######.......#######.....""".stripMargin


      new Screen().draw(bigInput) should be(expected)

    }

  }

  private val bigInput =
    """
      |addx 15
      |addx -11
      |addx 6
      |addx -3
      |addx 5
      |addx -1
      |addx -8
      |addx 13
      |addx 4
      |noop
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx -35
      |addx 1
      |addx 24
      |addx -19
      |addx 1
      |addx 16
      |addx -11
      |noop
      |noop
      |addx 21
      |addx -15
      |noop
      |noop
      |addx -3
      |addx 9
      |addx 1
      |addx -3
      |addx 8
      |addx 1
      |addx 5
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx -36
      |noop
      |addx 1
      |addx 7
      |noop
      |noop
      |noop
      |addx 2
      |addx 6
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx 1
      |noop
      |noop
      |addx 7
      |addx 1
      |noop
      |addx -13
      |addx 13
      |addx 7
      |noop
      |addx 1
      |addx -33
      |noop
      |noop
      |noop
      |addx 2
      |noop
      |noop
      |noop
      |addx 8
      |noop
      |addx -1
      |addx 2
      |addx 1
      |noop
      |addx 17
      |addx -9
      |addx 1
      |addx 1
      |addx -3
      |addx 11
      |noop
      |noop
      |addx 1
      |noop
      |addx 1
      |noop
      |noop
      |addx -13
      |addx -19
      |addx 1
      |addx 3
      |addx 26
      |addx -30
      |addx 12
      |addx -1
      |addx 3
      |addx 1
      |noop
      |noop
      |noop
      |addx -9
      |addx 18
      |addx 1
      |addx 2
      |noop
      |noop
      |addx 9
      |noop
      |noop
      |noop
      |addx -1
      |addx 2
      |addx -37
      |addx 1
      |addx 3
      |noop
      |addx 15
      |addx -21
      |addx 22
      |addx -6
      |addx 1
      |noop
      |addx 2
      |addx 1
      |noop
      |addx -10
      |noop
      |noop
      |addx 20
      |addx 1
      |addx 2
      |addx 2
      |addx -6
      |addx -11
      |noop
      |noop
      |noop
      |""".stripMargin

}
