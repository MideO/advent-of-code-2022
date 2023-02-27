package com.github.mideo
package exercises

import inputs.DistressSignalInput

class DistressSignalSpec extends TestSpec {

  "Packet" should {
    "read single digit line" in {
      Packet("[3]") should be(NestedPacket(IntPacket(3)))
    }
    "read empty line" in {
      Packet("[]") should be(NestedPacket())
    }

    "read simple line" in {
      Packet("[1,1,3,1,1]") should be(NestedPacket(IntPacket(1), IntPacket(1), IntPacket(3), IntPacket(1), IntPacket(1)))
    }

    "read nested line" in {
      Packet("[[1],[2,3,4]]") should be(
        NestedPacket(
          NestedPacket(IntPacket(1)),
          NestedPacket(IntPacket(2), IntPacket(3), IntPacket(4)))
      )
    }
    "read nested line with empty" in {
      Packet("[[],[2,3,4]]") should be(NestedPacket(NestedPacket(), NestedPacket(IntPacket(2), IntPacket(3), IntPacket(4))))
    }
    "read nested empty lines" in {
      Packet("[[[]]]") should be(NestedPacket(NestedPacket(NestedPacket())))
    }

    "read single nested line" in {
      Packet("[4,[5,6,7]]") should be(
        NestedPacket(
          IntPacket(4),
          NestedPacket(IntPacket(5), IntPacket(6), IntPacket(7))
        )
      )
      Packet("[3,[4,[5,6,7]]]") should be(
        NestedPacket(IntPacket(3),
          NestedPacket(IntPacket(4),
            NestedPacket(IntPacket(5), IntPacket(6), IntPacket(7)))))
    }

    "read multi nested line" in {
      Packet("[1,[2,[3,[4,[5,6,7]]]],8,9]") should be(
        NestedPacket(
          IntPacket(1),
          NestedPacket(
            IntPacket(2),
            NestedPacket(
              IntPacket(3),
              NestedPacket(IntPacket(4), NestedPacket(IntPacket(5), IntPacket(6), IntPacket(7)))
            )),
          IntPacket(8), IntPacket(9)
        )
      )
    }

    "read multi nested line with empty packets" in {

      Packet("[[],[[9,10,1,[9]]],[9,[],[[],[2,1,8,4],[3,4,1,3]],0]]") should be(
        NestedPacket(
          NestedPacket(),
          NestedPacket(NestedPacket(IntPacket(9), IntPacket(10), IntPacket(1), NestedPacket(IntPacket(9))))
          ,
          NestedPacket(
            IntPacket(9),
            NestedPacket(),
            NestedPacket(
              NestedPacket(),
              NestedPacket(IntPacket(2), IntPacket(1), IntPacket(8), IntPacket(4)),
              NestedPacket(IntPacket(3), IntPacket(4), IntPacket(1), IntPacket(3))
            ),
            IntPacket(0)
          )
        )
      )


    }


    "compareTo when packets are the same length" in {
      Packet("[1,1,5,1,1]").compareTo(Packet("[1,1,3,1,1]")) should be(1)
    }

    "compareTo when packets are the different length with left longer" in {
      Packet("[[1],[2,3,4]]").compareTo(Packet("[[1],4]")) should be(-1)
    }
    "compareTo when packets are the different length with right longer" in {
      Packet("[[1],4]").compareTo(Packet("[[1],[2,3,4]]")) should be(1)
    }
    "compareTo when complex packets are the different length with left shorter" in {
      Packet("[[2],[5,[1,[2,8,9],4,[4,1,7,8]],[],5],[9,9,[3,[10,2,9,0,4]],1],[5,[],8,[9,3,5,2]],[[[5],4,[0],[5],3],3,6]]")
        .compareTo(Packet("[[2,2,6],[5,6,[2,10]]]")) should be(-1)
    }
    "compareTo when complex packets are the different length with right shorter" in {
      Packet("[[2,2,6],[5,6,[2,10]]]")
        .compareTo(Packet("[[2],[5,[1,[2,8,9],4,[4,1,7,8]],[],5],[9,9,[3,[10,2,9,0,4]],1],[5,[],8,[9,3,5,2]],[[[5],4,[0],[5],3],3,6]]")) should be(1)
    }


    "compareTo when packets are the different length and shorter package is first" in {
      Packet("[9]").compareTo(Packet("[[8,7,6]]")) should be(1)

    }
    "compareTo when all elements are the same but left list is longer " in {
      Packet("[7,7,7,7]").compareTo(Packet("[7,7,7]")) should be(1)
    }
    "compareTo when all elements are the same but left list is longer and nested" in {
      Packet("[[4,4],4,4]").compareTo(Packet("[[4,4],4,4,4]")) should be(-1)
    }

    "compareTo when all elements are the same but right list is longer " in {
      Packet("[7,7,7]").compareTo(Packet("[7,7,7,7]")) should be(-1)
    }
    "compareTo when all elements are the same but right list is longer and nested" in {
      Packet("[[4,4],4,4,4]").compareTo(Packet("[[4,4],4,4]")) should be(1)
    }

    "compareTo when  left empty" in {
      Packet("[]").compareTo(Packet("[3]")) should be(-1)
    }
    "compareTo when a right empty" in {
      Packet("[3]").compareTo(Packet("[]")) should be(1)
    }
    "compareTo when both list are empty but left side is more nested" in {
      Packet("[[[]]]").compareTo(Packet("[[]]")) should be(1)
    }
    "compareTo when both list are empty but right side is more nested" in {
      Packet("[[]]").compareTo(Packet("[[[]]]")) should be(-1)
    }

    "compareTo " in {
      Packet("[[5,[[2],9],[5,7,[6,2,9,6],2]],[[2,5],4],[7,[2,4,[10],10],[0,2,9],[[],[6,4,10,8,0]],[]],[]]")
        .compareTo(Packet("[[[10,4,7,4],3],[[[2,10,3,10],2,3,0],[[8,9,5,10,8]]],[[]],[3,[1,[5,5,9,2],[1],[],[5,3,4]],3,2],[3,4]]")
        ) should be(-1)


    }

    "Packets" should {
      "return position of ordered pairs" in {
        val pair1 = (Packet("[1,1,3,1,1]"), Packet("[1,1,5,1,1]"))
        val pair2 = (Packet("[[1],[2,3,4]]"), Packet("[[1],4]"))
        val pair3 = (Packet("[9]"), Packet("[[8,7,6]]"))
        val pair4 = (Packet("[[4,4],4,4]"), Packet("[[4,4],4,4,4]"))
        val pair5 = (Packet("[7,7,7,7]"), Packet("[7,7,7]"))
        val pair6 = (Packet("[]"), Packet("[3]"))
        val pair7 = (Packet("[[[]]]"), Packet("[[]]"))
        val pair8 = (Packet("[1,[2,[3,[4,[5,6,7]]]],8,9]"), Packet("[1,[2,[3,[4,[5,6,0]]]],8,9]"))

        Packets.ordered(pair1, pair2, pair3, pair4, pair5, pair6, pair7, pair8) should contain theSameElementsInOrderAs
          Seq(1, 2, 4, 6)
      }
    }

    "DistressSignal" should {


      "return sum of ordered pairs" in {

        val ordered = DistressSignal.ordered(DistressSignalInput.value)
        ordered should be(
          List(1, 2, 3, 6, 7, 9, 10, 11, 12, 15, 16, 18, 19, 21, 22, 24, 28, 30, 31, 32, 33, 35, 37, 38, 40, 42, 43, 44, 47, 48, 49, 51, 54, 59, 60, 61, 62, 64, 69, 70, 71, 75, 76, 77, 81, 82, 84, 86, 88, 90, 93, 94, 97, 98, 101, 102, 104, 105, 106, 108, 110, 111, 112, 113, 115, 118, 119, 121, 127, 128, 129, 132, 134, 135, 136, 138, 139, 140, 141, 144, 145, 146, 147, 148, 149, 150))

        ordered.sum should be(6568)

      }

      "return sorted of ordered pairs" in {
        val input =
          """
            |[1,1,3,1,1]
            |[1,1,5,1,1]
            |
            |[[1],[2,3,4]]
            |[[1],4]
            |
            |[9]
            |[[8,7,6]]
            |
            |[[4,4],4,4]
            |[[4,4],4,4,4]
            |
            |[7,7,7,7]
            |[7,7,7]
            |
            |[]
            |[3]
            |
            |[[[]]]
            |[[]]
            |
            |[1,[2,[3,[4,[5,6,7]]]],8,9]
            |[1,[2,[3,[4,[5,6,0]]]],8,9]
            |""".stripMargin
        val markers = List(NestedPacket(NestedPacket(IntPacket(2))), NestedPacket(NestedPacket(IntPacket(6))))

        DistressSignal.position(DistressSignal.sorted(input, markers), markers) should be(Seq(10, 14))

      }
    }
  }
}
