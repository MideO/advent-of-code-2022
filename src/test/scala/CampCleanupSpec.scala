package com.github.mideo

import exercises.{CampCleanup, ElfPair, Section}

class CampCleanupSpec extends TestSpec {

  "Section" should {
    "return block sequence" in {
      Section("0-9").sequence should be(0 to 9)
    }
  }

  "ElfPair" should {
    "return true when a section fully contains another section" in {
      ElfPair(Section("2-8"), Section("3-7")).isFullyContained should be(true)
      ElfPair(Section("6-6"), Section("4-6")).isFullyContained should be(true)
    }

    "return false when a section does not fully contains another section" in {
      ElfPair(Section("2-4"), Section("6-8")).isFullyContained should be(false)
      ElfPair(Section("5-7"), Section("7-9")).isFullyContained should be(false)
    }

    "return true when a section overlaps another section" in {
      ElfPair(Section("5-7"), Section("7-9")).isOverlapping should be(true)
      ElfPair(Section("2-8"), Section("3-7")).isOverlapping should be(true)
      ElfPair(Section("6-6"), Section("4-6")).isOverlapping should be(true)
    }

    "return false when a section does not overlaps another section" in {
      ElfPair(Section("2-4"), Section("6-8")).isOverlapping should be(false)
      ElfPair(Section("2-3"), Section("4-5")).isOverlapping should be(false)


    }
  }

  "CampCleanup" should {
    val input =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin
    "return total count of Elf Pairs with fully contained sections" in {
      CampCleanup.countPairs(input, pair => pair.isFullyContained) should be(2)
    }
    "return total count of Elf Pairs with overlapping sections" in {
      CampCleanup.countPairs(input, pair => pair.isOverlapping) should be(4)
    }

  }


}
