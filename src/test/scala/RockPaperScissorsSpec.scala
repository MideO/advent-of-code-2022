package com.github.mideo

import RockPaperScissors._

class RockPaperScissorsSpec extends TestSpec {
  "RockPaperScissors Player" should {


    "Follow suspicious GameOption Strategy Guide" in {
      val strategy =
        """
          |A Y
          |B Z
          |C X
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithGameOptionStrategy) should be(24)
    }


    "Follow Non-suspicious GameOption Strategy Guide" in {
      val strategy =
        """
          |A Y
          |B X
          |C Z
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithGameOptionStrategy) should be(15)
    }

    "Follow All Loss GameOption Strategy Guide" in {
      val strategy =
        """
          |A Z
          |B X
          |C Y
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithGameOptionStrategy) should be(6)
    }

    "Follow All Draws GameOption Strategy Guide" in {
      val strategy =
        """
          |A X
          |B Y
          |C Z
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithGameOptionStrategy) should be(15)
    }

    "Follow All Outcome Strategy Guide" in {
      val strategy =
        """
          |A Y
          |B X
          |C Z
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithOutcomeStrategy) should be(12)
    }

    "Follow All Draws Outcome Strategy Guide" in {
      val strategy =
        """
          |A Y
          |B Y
          |C Y
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithOutcomeStrategy) should be(15)
    }

    "Follow All Wins Outcome Strategy Guide" in {
      val strategy =
        """
          |A Z
          |B Z
          |C Z
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithOutcomeStrategy) should be(24)
    }

    "Follow All Loss Outcome Strategy Guide" in {
      val strategy =
        """
          |A X
          |B X
          |C X
          |""".stripMargin

      RockPaperScissors.playWithStrategy(strategy, PlayWithOutcomeStrategy) should be(6)
    }

  }
}
