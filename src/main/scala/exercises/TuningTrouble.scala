package com.github.mideo
package exercises

import scala.collection.mutable.Stack
import scala.util.control.Breaks.{break, breakable}

// https://adventofcode.com/2022/day/6
case class MessageDecoder(input: String, numberOfUniqueCharacters: Int) {
  def beginIndex: Int = {
    val stack = new Stack[(Char, Int)]
    breakable {
      input.toCharArray
        .zipWithIndex
        .foreach {
          case x if stack.isEmpty => stack.push(x)
          case x if stack.exists(y => y._1 == x._1) =>
            while (stack.exists(y => y._1 == x._1)) stack.removeLast(true)
            stack.push(x)

          case x if stack.size < numberOfUniqueCharacters => stack.push(x)
          case _ => break()
        }
    }
    stack.head._2 + 1
  }

}

sealed trait MessageMarkerIndex {
  val index: Int
}

object StartOfPacketMarker extends MessageMarkerIndex {
  override val index: Int = 4
}

object StartOfMessageMarker extends MessageMarkerIndex {
  override val index: Int = 14
}

object TuningTrouble {
  def startMarkerIndex(input: String, marker: MessageMarkerIndex): Int = {
    MessageDecoder(input, marker.index).beginIndex
  }

}

