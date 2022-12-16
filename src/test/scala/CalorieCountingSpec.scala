package com.github.mideo

import com.github.mideo.CalorieCounting.Elf
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class CalorieCountingSpec extends AnyWordSpec with Matchers {

  "Elves carrying the most calories " should {
    "return total number of calories carry by the elf with most calories"  in {

      val caloriesList  =
        """
          |1000
          |2000
          |3000
          |
          |4000
          |
          |5000
          |6000
          |
          |7000
          |8000
          |9000
          |
          |10000
          |""".stripMargin


      CalorieCounting.getElfMostCalories(caloriesList) should be(Some(Elf(4, 24000)))

    }

    "return total number of calories carry by the first elf with most calories" in {

      val caloriesList =
        """
          |7000
          |8000
          |9000
          |
          |1000
          |2000
          |3000
          |
          |4000
          |
          |5000
          |6000
          |
          |7000
          |8000
          |9000
          |""".stripMargin


      CalorieCounting.getElfMostCalories(caloriesList) should be(Some(Elf(1, 24000)))

    }

    "return nothing when given empty list" in {
      CalorieCounting.getElfMostCalories("")  should be(None)
    }
  }
}
