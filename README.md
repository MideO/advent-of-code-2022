## advent-of-code-2022
See: https://adventofcode.com/2022

### Dependencies
* SBT 1.8.0
* Scala 2.13

### How to Run Tests
```bash
  sbt clean test
```

### How to use
Create an input in [CalorieCounting.scala](./src/main/scala/inputs/CalorieCountingInput.scala)
 
```
object CalorieCountingInput {
  val value ="""
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
}
```

Evaluate Result in [Main.scala](./src/main/scala/Main.scala)
```
  // Day 1
  // Q1
  print (
    "CalorieCounting.getElfMostCalories:",
    CalorieCounting.getElfMostCalories(Inputs.CalorieCountingInput)
  )
  // Q2
  print(
    "CalorieCounting.getTopNElvesMostCalories",
    CalorieCounting.getTopNElvesMostCalories(Inputs.CalorieCountingInput, 3).map(_.totalCalories).sum
  )

```
Run Main
```bash
  sbt clean run
```