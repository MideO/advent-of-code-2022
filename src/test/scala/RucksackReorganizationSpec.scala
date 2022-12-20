package com.github.mideo

class RucksackReorganizationSpec extends TestSpec {
  "RuckSack" should {

    "split items into two equal compartment" in {
      RuckSack("vJrwpWtwJgWrhcsFMMfFFhFp") should be(
        RuckSack(RucksackCompartment("vJrwpWtwJgWr"), RucksackCompartment("hcsFMMfFFhFp"))
      )
      RuckSack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL") should be(
        RuckSack(RucksackCompartment("jqHRNqRjqzjGDLGL"), RucksackCompartment("rsFMfFZSrLrFZsSL"))
      )
      RuckSack("PmmdzqPrVvPwwTWBwg") should be(
        RuckSack(RucksackCompartment("PmmdzqPrV"), RucksackCompartment("vPwwTWBwg"))
      )
      RuckSack("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn") should be(
        RuckSack(RucksackCompartment("wMqvLMZHhHMvwLH"), RucksackCompartment("jbvcjnnSBnvTQFn"))
      )
      RuckSack("ttgJtRGJQctTZtZT") should be(
        RuckSack(RucksackCompartment("ttgJtRGJ"), RucksackCompartment("QctTZtZT"))
      )
      RuckSack("CrZsJsPPZsGzwwsLwLmpwMDw") should be(
        RuckSack(RucksackCompartment("CrZsJsPPZsGz"), RucksackCompartment("wwsLwLmpwMDw"))
      )
    }

    "find common items in two compartment" in {
      RuckSack("vJrwpWtwJgWrhcsFMMfFFhFp").findCommon should be(Option("p"))
      RuckSack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL").findCommon should be(Option("L"))
      RuckSack("PmmdzqPrVvPwwTWBwg").findCommon should be(Option("P"))
      RuckSack("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn").findCommon should be(Option("v"))
      RuckSack("ttgJtRGJQctTZtZT").findCommon should be(Option("t"))
      RuckSack("CrZsJsPPZsGzwwsLwLmpwMDw").findCommon should be(Option("s"))
    }

  }
  "ElfGroup" should {
    "find common items in all Rucksacks" in {
      val group1 = ElfGroup(RuckSack("vJrwpWtwJgWrhcsFMMfFFhFp"), RuckSack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"), RuckSack("PmmdzqPrVvPwwTWBwg"))
      group1.findCommon should be(Some("r"))

      val group2 = ElfGroup(RuckSack("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"), RuckSack("ttgJtRGJQctTZtZT"), RuckSack("CrZsJsPPZsGzwwsLwLmpwMDw"))
      group2.findCommon should be(Some("Z"))
    }
  }
  "RucksackReorganization" should {
    "have defined priorities" in {
      RucksackReorganization.itemPriority("p") should be(16)
      RucksackReorganization.itemPriority("L") should be(38)
      RucksackReorganization.itemPriority("P") should be(42)
      RucksackReorganization.itemPriority("v") should be(22)
      RucksackReorganization.itemPriority("t") should be(20)
      RucksackReorganization.itemPriority("s") should be(19)
      RucksackReorganization.itemPriority("r") should be(18)
      RucksackReorganization.itemPriority("Z") should be(52)
    }

    "sum up priority items" in {
      val input =
        """
          |vJrwpWtwJgWrhcsFMMfFFhFp
          |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
          |PmmdzqPrVvPwwTWBwg
          |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
          |ttgJtRGJQctTZtZT
          |CrZsJsPPZsGzwwsLwLmpwMDw
          |""".stripMargin

      RucksackReorganization.sumPriority(input) should be(157)
    }

    "sum up priority items for all UpperCase" in {
      val input =
        """
          |ABCDEFGJIHKLMNAPQRSTUVWXYZ
          |ABCDEFGJZHKLMNOPQRSTUVWXYZ
          |""".stripMargin

      RucksackReorganization.sumPriority(input) should be(79)
    }
    "sum up priority items for all LowerCase" in {
      val input =
        """
          |abcdefghijklmnjpqrstuvwxyz
          |abcdzfghijklmnopqrstuvwxyz
          |""".stripMargin

      RucksackReorganization.sumPriority(input) should be(36)
    }

    "sum up priority by elf groups" in {
      val input =
        """
          |vJrwpWtwJgWrhcsFMMfFFhFp
          |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
          |PmmdzqPrVvPwwTWBwg
          |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
          |ttgJtRGJQctTZtZT
          |CrZsJsPPZsGzwwsLwLmpwMDw
          |""".stripMargin

      RucksackReorganization.sumPriorityByElfGroup(input) should be(70)
    }
  }
}
