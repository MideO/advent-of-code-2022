package com.github.mideo
package exercises

import scala.collection.mutable.Stack

// https://adventofcode.com/2022/day/5
class SupplyStacksSpec extends TestSpec {
  val itemN = StackItem("N")
  val itemM = StackItem("M")
  val itemO = StackItem("O")
  val itemZ = StackItem("Z")

  "SupplyStack" should {

    "return top StackItem" in {
      SupplyStack(Stack(itemN, itemM, itemZ)).top should be(itemN)
    }
  }

  "SingleStackCrateMover" should {
    "return StackItem with one item" in {
      val stack = SupplyStack()
      SingleStackCrateMover.put(stack, itemN)
      SingleStackCrateMover.get(stack, 1) should be(Seq(itemN))
    }

    "return StackItems" in {
      val stack = SupplyStack()
      SingleStackCrateMover.put(stack, itemN, itemM)
      SingleStackCrateMover.put(stack, itemZ)
      SingleStackCrateMover.get(stack, 1) should be(Seq(itemZ))

    }

    "return top N StackItems" in {
      val stack = SupplyStack()
      SingleStackCrateMover.put(stack, itemN, itemM)
      SingleStackCrateMover.put(stack, itemZ, itemO)
      SingleStackCrateMover.get(stack, 2) should be(Seq(itemO, itemZ))
    }


    "return top StackItem after consecutive puts" in {
      val stack = SupplyStack()
      SingleStackCrateMover.put(stack, itemN)
      SingleStackCrateMover.put(stack, itemM)
      SingleStackCrateMover.put(stack, itemZ)
      stack.top should be(itemZ)

    }


    "move stack item from one stack to another" in {
      val stacks = Map(
        1 -> SupplyStack(Stack(itemN)),
        2 -> SupplyStack(Stack(itemZ, itemO, itemM))
      )

      SingleStackCrateMover.move("move 1 from 2 to 1", stacks)

      stacks(1).top should be(itemZ)
      SingleStackCrateMover.get(stacks(1), 2) should be(Seq(itemZ, itemN))
    }

    "move two stack items from one stack to another" in {
      val stacks = Map(
        1 -> SupplyStack(Stack(itemN)),
        2 -> SupplyStack(Stack((1 to 20).map(x => StackItem(x.toString)).reverse: _*))
      )

      SingleStackCrateMover.move("move 15 from 2 to 1", stacks)

      stacks(1).top should be(StackItem("6"))

      SingleStackCrateMover.move("move 16 from 1 to 2", stacks)

      stacks(2).top should be(StackItem("N"))
      SingleStackCrateMover.get(stacks(2), 3) should be(Seq(StackItem("N"), StackItem("20"), StackItem("19")))
    }
  }

  "MultiStackCrateMover" should {
    "return StackItem with one item" in {
      val stack = SupplyStack()
      MultiStackCrateMover.put(stack, itemN)
      MultiStackCrateMover.get(stack, 1) should be(Seq(itemN))
    }

    "return StackItems" in {
      val stack = SupplyStack()
      MultiStackCrateMover.put(stack, itemN, itemM)
      MultiStackCrateMover.put(stack, itemZ)
      SingleStackCrateMover.get(stack, 1) should be(Seq(itemZ))

    }

    "return top N StackItems" in {
      val stack = SupplyStack()
      MultiStackCrateMover.put(stack, itemN, itemM)
      MultiStackCrateMover.put(stack, itemZ, itemO)
      stack.items.toSeq should be(Seq(itemZ, itemO,itemN, itemM))
      MultiStackCrateMover.get(stack, 2) should be(Seq(itemZ, itemO))
    }


    "return top StackItem after consecutive puts" in {
      val stack = SupplyStack()
      MultiStackCrateMover.put(stack, itemN)
      MultiStackCrateMover.put(stack, itemM)
      MultiStackCrateMover.put(stack, itemZ)
      stack.top should be(itemZ)

    }


    "move stack item from one stack to another" in {
      val stacks = Map(
        1 -> SupplyStack(Stack(itemN)),
        2 -> SupplyStack(Stack(itemZ, itemO, itemM))
      )

      MultiStackCrateMover.move("move 1 from 2 to 1", stacks)

      stacks(1).top should be(itemZ)
      MultiStackCrateMover.get(stacks(1), 2) should be(Seq(itemZ, itemN))
    }

    "move two stack items from one stack to another" in {
      val stacks = Map(
        1 -> SupplyStack(Stack(itemN)),
        2 -> SupplyStack(Stack((1 to 20).map(x => StackItem(x.toString)).reverse: _*))
      )

      MultiStackCrateMover.move("move 15 from 2 to 1", stacks)

      stacks(1).top should be(StackItem("20"))

      MultiStackCrateMover.move("move 16 from 1 to 2", stacks)

      stacks(2).top should be(StackItem("20"))
      MultiStackCrateMover.get(stacks(2), 3) should be(Seq(StackItem("20"), StackItem("19"), StackItem("18")))
    }
  }

  "Stacks" should {

    "an input to stacks" in {
      val input =
        """
          |    [D]
          |[N] [C]
          |[Z] [M] [P]
          | 1   2   3
          |""".stripMargin
      val result = Stacks(input)

      result(1).items.toSeq should be(Seq(StackItem("N"), StackItem("Z")))

      result(2).items.toSeq should be(Seq(StackItem("D"), StackItem("C"), StackItem("M")))

      result(3).items.toSeq should be(Seq(StackItem("P")))


    }

    "sort bigger stack based on instructions" in {
      val input =
        """
          |                [V]     [C]     [M]
          |[V]     [J]     [N]     [H]     [V]
          |[R] [F] [N]     [W]     [Z]     [N]
          |[H] [R] [D]     [Q] [M] [L]     [B]
          |[B] [C] [H] [V] [R] [C] [G]     [R]
          |[G] [G] [F] [S] [D] [H] [B] [R] [S]
          |[D] [N] [S] [D] [H] [G] [J] [J] [G]
          |[W] [J] [L] [J] [S] [P] [F] [S] [L]
          | 1   2   3   4   5   6   7   8   9
          |
          |""".stripMargin

      val result = Stacks(input)
      result(1).items.toSeq should be(Seq(StackItem("V"), StackItem("R"), StackItem("H"), StackItem("B"), StackItem("G"), StackItem("D"), StackItem("W")))
      result(2).items.toSeq should be(Seq(StackItem("F"), StackItem("R"), StackItem("C"), StackItem("G"), StackItem("N"), StackItem("J")))
      result(3).items.toSeq should be(Seq(StackItem("J"), StackItem("N"), StackItem("D"), StackItem("H"), StackItem("F"), StackItem("S"), StackItem("L")))
      result(4).items.toSeq should be(Seq(StackItem("V"), StackItem("S"), StackItem("D"), StackItem("J")))
      result(5).items.toSeq should be(Seq(StackItem("V"), StackItem("N"), StackItem("W"), StackItem("Q"), StackItem("R"), StackItem("D"), StackItem("H"), StackItem("S")))
      result(6).items.toSeq should be(Seq(StackItem("M"), StackItem("C"), StackItem("H"), StackItem("G"), StackItem("P")))
      result(7).items.toSeq should be(Seq(StackItem("C"), StackItem("H"), StackItem("Z"), StackItem("L"), StackItem("G"), StackItem("B"), StackItem("J"), StackItem("F")))
      result(8).items.toSeq should be(Seq(StackItem("R"), StackItem("J"), StackItem("S")))
      result(9).items.toSeq should be(Seq(StackItem("M"), StackItem("V"), StackItem("N"), StackItem("B"), StackItem("R"), StackItem("S"), StackItem("G"), StackItem("L")))


    }
    "SupplyStacks" should {

      "sort stack based instructions with SingleStackCrateMover" in {
        implicit val crane =  SingleStackCrateMover
        val input =
          """
            |    [D]
            |[N] [C]
            |[Z] [M] [P]
            | 1   2   3
            |
            |move 1 from 2 to 1
            |move 3 from 1 to 3
            |move 2 from 2 to 1
            |move 1 from 1 to 2
            |move 4 from 3 to 1
            |move 1 from 1 to 2
            |move 1 from 1 to 3
            |""".stripMargin


        SupplyStacks.topItems(input) should be("NPD")
      }

      "sort stack based instructions with MultiStackCrateMover" in {
        implicit val crane = MultiStackCrateMover
        val input =
          """
            |    [D]
            |[N] [C]
            |[Z] [M] [P]
            | 1   2   3
            |
            |move 1 from 2 to 1
            |move 3 from 1 to 3
            |move 2 from 2 to 1
            |move 1 from 1 to 2
            |""".stripMargin


        SupplyStacks.topItems(input) should be("MCD")
      }
    }

  }
}
