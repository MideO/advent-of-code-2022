package com.github.mideo
package exercises

import scala.math.BigInt

//https://adventofcode.com/2022/day/11
class MonkeyInTheMiddleSpec extends TestSpec {
  "Item" should {

    "return false if not divisibleBY" in {
      Item(5).isDivisibleBy(2) should be(false)
    }
  }


  "ItemInspectionCounter" should {
    "increment " in {
      val counter = new ItemInspectionCounter
      counter.inc
      counter.get should be(1)
      counter.inc
      counter.get should be(2)
    }
  }

  "Collection" should {

    "get first item" in {
      new Collection[Int](1, 2, 3).head should be(1)
    }

    "return true when items is not empty" in {
      new Collection[Int](1, 2, 3).nonEmpty should be(true)
    }
    "return true when items is empty" in {
      new Collection[Int]().isEmpty should be(true)
    }
    "get and remove first item" in {
      val items = new Collection[Int](1, 2, 3)
      items.head should be(1)
      assert(items.equals(new Collection(2, 3)))
    }
    "add item as last" in {
      val items = new Collection[Int](1, 2, 3)
      items.add(4)
      assert(items.equals(new Collection(1, 2, 3, 4)))
    }
  }

  "ThrowAndReceiveItem" should {
    "throw and receive item" in {
      val entity = new ThrowAndReceiveItem[Int] {
        override val Items: Collection[Int] = new Collection[Int](0, 1, 2)
      }
      val otherEntity = new ThrowAndReceiveItem[Int] {
        override val Items: Collection[Int] = new Collection[Int]()
      }

      entity.throwItem(0, otherEntity)
      entity.throwItem(1, otherEntity)

      assert(otherEntity.all.equals(new Collection(0, 1)))
    }

  }
  "Monkeys" should {
    "add monkeys" in {
      val monkeys = new Monkeys
      val monkey0 = new Monkey(
        monkeys = monkeys,
        options = (1, 2),
        "*","19", 3,23,
        items = Item(79), Item(98)
      )
      monkeys.add(0, monkey0)
      monkeys.get(0) should be(monkey0)
    }
    "map monkeys" in {
      val monkeys = new Monkeys
      val monkey0 = new Monkey(
        monkeys = monkeys,
        options = (1, 2),
        "*","19", 3,23,
        items = Item(79), Item(98)
      )
      monkeys.add(0, monkey0)
      val result = monkeys.map(it => it.Items)
      result.size should be(1)
      assert(result(0).equals(new Collection(Item(79), Item(98))))

    }

    "load monkeys from input string" in {
      val input =
        """
          |Monkey 0:
          |  Starting items: 79, 98
          |  Operation: new = old * 19
          |  Test: divisible by 23
          |    If true: throw to monkey 2
          |    If false: throw to monkey 3
          |
          |Monkey 1:
          |  Starting items: 54, 65, 75, 74
          |  Operation: new = old + 6
          |  Test: divisible by 19
          |    If true: throw to monkey 2
          |    If false: throw to monkey 0
          |
          |Monkey 2:
          |  Starting items: 79, 60, 97
          |  Operation: new = old * old
          |  Test: divisible by 13
          |    If true: throw to monkey 1
          |    If false: throw to monkey 3
          |
          |Monkey 3:
          |  Starting items: 74
          |  Operation: new = old + 3
          |  Test: divisible by 17
          |    If true: throw to monkey 0
          |    If false: throw to monkey 1
          |""".stripMargin

      val monkeys = Monkeys(input, 3)
      assert(monkeys.get(0).equals(new Monkey(
        monkeys = monkeys,
        options = (2, 3),
        "*",
        "19",
        3,
        23,
        items = Item(79), Item(98)
      )))
      assert(monkeys.get(1).equals(new Monkey(
        monkeys = monkeys,
        options = (2, 0),
        "+", "6", 3, 19,
        items = Item(54), Item(65), Item(75), Item(74)
      )))
      assert(monkeys.get(2).equals(new Monkey(
        monkeys = monkeys,
        options = (1, 3),
        "*",
        "old",
        3, 13,
        items = Item(79), Item(60), Item(97)
      )))
      assert(monkeys.get(3).equals(new Monkey(
        monkeys = monkeys,
        options = (0, 1),
        "+","3", 3,17,
        items = Item(74)
      )))

    }
  }
  "Monkey" should {
    "Monkey0 throws Item to another based on decision" in {
      val monkeys = new Monkeys
      val monkey0 = new Monkey(
        monkeys = monkeys,
        options = (2, 3),
        "*","19", 3,23,
        items = Item(79), Item(98)
      )
      monkeys.add(0, monkey0)
      val monkey2 = new Monkey(
        monkeys = monkeys,
        options = (1, 2),
        "*","old", 3,13,
        items = Item(79), Item(60), Item(97)
      )
      monkeys.add(2, monkey2)
      val monkey3 = new Monkey(
        monkeys = monkeys,
        options = (1, 2),
        "+","3", 3,17,
        items = Item(74)
      )
      monkeys.add(3, monkey3)

      monkey0.throwItems()
      monkey0.Items.isEmpty should be(true)
      monkey0.InspectionCounter.get should be(2)
      assert(monkey2.Items.equals(new Collection(Item(79), Item(60), Item(97))))
      assert(monkey3.Items.equals(new Collection(Item(74), Item(201), Item(22))))
    }

    "Monkey1 throws Item to another based on decision" in {
      val monkeys = new Monkeys
      val monkey0 = new Monkey(
        monkeys = monkeys,
        options = (1, 2),
        "*","19", 3,23,
        items = Item(79), Item(98)
      )
      monkeys.add(0, monkey0)
      val monkey1 = new Monkey(
        monkeys = monkeys,
        options = (2, 0),
        "+", "6", 3, 19,
        items = Item(54), Item(65), Item(75), Item(74)
      )
      monkeys.add(1, monkey1)

      val monkey2 = new Monkey(
        monkeys = monkeys,
        options = (1, 2),
        "+","3", 3,17,
        items = Item(79), Item(60), Item(97)
      )
      monkeys.add(2, monkey2)

      monkey1.throwItems()
      monkey1.Items.nonEmpty should be(false)
      monkey1.InspectionCounter.get should be(4)
      assert(
        monkey2.Items.equals(
          new Collection(Item(79), Item(60), Item(97))
        )
      )
      assert(
        monkey0.Items.equals(
          new Collection(Item(79), Item(98), Item(20), Item(23), Item(27), Item(26)))
      )
    }

    "create and add monkey from text" in {
      val monkeys = new Monkeys
      val input =
        """
          |Monkey 0:
          |  Starting items: 79, 98
          |  Operation: new = old * 19
          |  Test: divisible by 23
          |    If true: throw to monkey 2
          |    If false: throw to monkey 3
          |""".stripMargin
      val monkey = Monkey(input, monkeys, 3)

      assert(monkey.equals(new Monkey(
        monkeys = monkeys,
        options = (2, 3),
        "*","19", 3,23,
        items = Item(79), Item(98)
      )))
      assert(monkeys.get(0).equals(monkey))

    }
  }

  "MonkeyInTheMiddle" should {
    "play input with number of rounds with stress reducer" in {
      val input =
        """
          |Monkey 0:
          |  Starting items: 79, 98
          |  Operation: new = old * 19
          |  Test: divisible by 23
          |    If true: throw to monkey 2
          |    If false: throw to monkey 3
          |
          |Monkey 1:
          |  Starting items: 54, 65, 75, 74
          |  Operation: new = old + 6
          |  Test: divisible by 19
          |    If true: throw to monkey 2
          |    If false: throw to monkey 0
          |
          |Monkey 2:
          |  Starting items: 79, 60, 97
          |  Operation: new = old * old
          |  Test: divisible by 13
          |    If true: throw to monkey 1
          |    If false: throw to monkey 3
          |
          |Monkey 3:
          |  Starting items: 74
          |  Operation: new = old + 3
          |  Test: divisible by 17
          |    If true: throw to monkey 0
          |    If false: throw to monkey 1
          |""".stripMargin
      val monkeys = MonkeyInTheMiddle.play(input, 20)
      monkeys.get(0).InspectionCounter.get should be(101)
      monkeys.get(1).InspectionCounter.get should be(95)
      monkeys.get(2).InspectionCounter.get should be(7)
      monkeys.get(3).InspectionCounter.get should be(105)
      MonkeyInTheMiddle.monkeyBusinessLevel(monkeys) should be(BigInt(10605))

    }
    "play input with number of rounds without stress reducer" in {
      val input =
        """
          |Monkey 0:
          |  Starting items: 79, 98
          |  Operation: new = old * 19
          |  Test: divisible by 23
          |    If true: throw to monkey 2
          |    If false: throw to monkey 3
          |
          |Monkey 1:
          |  Starting items: 54, 65, 75, 74
          |  Operation: new = old + 6
          |  Test: divisible by 19
          |    If true: throw to monkey 2
          |    If false: throw to monkey 0
          |
          |Monkey 2:
          |  Starting items: 79, 60, 97
          |  Operation: new = old * old
          |  Test: divisible by 13
          |    If true: throw to monkey 1
          |    If false: throw to monkey 3
          |
          |Monkey 3:
          |  Starting items: 74
          |  Operation: new = old + 3
          |  Test: divisible by 17
          |    If true: throw to monkey 0
          |    If false: throw to monkey 1
          |""".stripMargin
      val monkeys = MonkeyInTheMiddle.play(input, 10000, 1)
      monkeys.get(0).InspectionCounter.get should be(52166)
      monkeys.get(1).InspectionCounter.get should be(47830)
      monkeys.get(2).InspectionCounter.get should be(1938)
      monkeys.get(3).InspectionCounter.get should be(52013)
      MonkeyInTheMiddle.monkeyBusinessLevel(monkeys) should be(2713310158L)
    }

  }

}
