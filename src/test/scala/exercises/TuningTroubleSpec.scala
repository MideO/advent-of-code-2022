package com.github.mideo
package exercises

import org.scalatest.prop.TableDrivenPropertyChecks._

class TuningTroubleSpec extends TestSpec {
  "MessageDecoder" should {
    val messagesAndBeginning =
      Table(
        ("message", "beginPacket", "beginMessage"),
        ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 25),
        ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
        ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
        ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
        ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
      )
    "find message Start Of Packet Marker" in {
      forAll(messagesAndBeginning) {
        (message, beginPacket, _) => MessageDecoder(message, StartOfPacketMarker.index).beginIndex should be(beginPacket)
      }
    }
    "find message Start Of Message Marker" in {
      forAll(messagesAndBeginning) {
        (message, _, beginMessage) => MessageDecoder(message, StartOfMessageMarker.index).beginIndex should be(beginMessage)
      }
    }
  }
  "TuningTrouble" should {
    "find message Start Of Packet Marker" in {
      TuningTrouble.startMarkerIndex("mjqjpqmgbljsphdztnvjfqwrcgsmlb", StartOfPacketMarker) should be(7)
    }
    "find message Start Of Message Marker" in {
      TuningTrouble.startMarkerIndex("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", StartOfMessageMarker) should be(29)
    }
  }


}
