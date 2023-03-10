package com.github.mideo

package object exercises {
  case class Position(x: Int, y: Int)

  def iterator(input:String): Iterator[String] = input.linesIterator.filter(_.nonEmpty)

  def numberRange(a: Int, b: Int) = a to b by (if (a > b) -1 else 1)
}
