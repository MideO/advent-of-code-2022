package com.github.mideo
package exercises

class CalorieCountingSpec extends TestSpec {

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


      CalorieCounting.getElfMostCalories(caloriesList) should be(Some(Elf(24000)))

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


      CalorieCounting.getElfMostCalories(caloriesList) should be(Some(Elf(24000)))

    }

    "return nothing when given empty list" in {
      CalorieCounting.getElfMostCalories("")  should be(None)
    }


    "return top three the elf with most calories" in {
      val caloriesList =
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



      val result = CalorieCounting.getTopNElvesMostCalories(caloriesList, 3)
      result should be(List(
        Elf(24000),
        Elf(11000),
        Elf(10000))
      )
      result.map(_.totalCalories).sum should be(45000)

    }

    "return top three the elf with most calories when there elves with same calories" in {
      val caloriesList =
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
          |7000
          |8000
          |9000
          |""".stripMargin


      CalorieCounting.getTopNElvesMostCalories(caloriesList, 3) should be(List(
        Elf(24000),
        Elf(24000),
        Elf(11000))
      )

    }
  }
}
