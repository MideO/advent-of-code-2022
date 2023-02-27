package com.github.mideo
package exercises

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.Stack

// https://adventofcode.com/2022/day/13

sealed trait Packet extends Comparable[Packet] {

  def `+`(packet: Packet): Packet

}

case class IntPacket(value: Int) extends Packet {

  override def `+`(packet: Packet): Packet = NestedPacket(this, packet)

  override def compareTo(that: Packet): Int = that match {
    case IntPacket(value) => this.value.compareTo(value)
    case x => NestedPacket(this).compareTo(x)
  }

}

case class NestedPacket(packets: Packet*) extends Packet {

  override def `+`(packet: Packet): Packet = NestedPacket(packets :+ packet: _*)

  override def compareTo(that: Packet): Int = that match {
    case IntPacket(value) => compare(packets.toList, Seq(IntPacket(value)))
    case NestedPacket(pks@_*) => compare(packets.toList, pks.toList)
  }

  @tailrec
  private def compare(packets: Seq[Packet], other: Seq[Packet]): Int = (packets, other) match {
    case (Nil, Nil) => 0
    case (Nil, _ :: _) => -1
    case (_ :: _, Nil) => 1
    case (x :: xs, y :: ys) => x.compareTo(y) match {
      case 0 => compare(xs, ys)
      case x => x
    }
  }
}

case class State(number: Option[Int] = None, values: Queue[Packet] = Queue.empty) {
  def handle(digit: Int): State = this.copy(number = number.map(_ * 10 + digit).orElse(Option(digit)))

  def commit: State = number
    .map(it => State(None, values = this.values :+ IntPacket(it)))
    .getOrElse(this)
}


object Packet {

  def apply(input: String, state: State = State(), stack: List[Queue[Packet]] = Nil): Packet = {
    if (input.isEmpty) return state.commit.values.head
    input.headOption match {
      case Some('[') => apply(input.tail, State(), state.values :: stack)
      case Some(']') =>
        val packet = NestedPacket(state.commit.values.toList: _*)
        stack match {
          case x :: xs => apply(input.tail, State(values = x :+ packet), xs)
          case Nil => packet
        }
      case Some(',') => apply(input.tail, state.commit, stack)
      case Some(n) =>
        apply(input.tail, state.handle(n.asDigit), stack)
    }
  }

}

object Packets {
  def ordered(pairs: (Packet, Packet)*): Seq[Int] = pairs.toSeq
    .indices
    .filter(index => pairs(index)._1.compareTo(pairs(index)._2) <= 0)
    .map(_ + 1)

}

object DistressSignal {
  def ordered(input: String): Seq[Int] = {
    val pairs = input.split("\n\n")
      .map {
        it =>
          val pair = it.split("\n").filter(_.nonEmpty)
          (Packet(pair.head), Packet(pair.last))
      }
    Packets.ordered(pairs: _*)
  }

  def sorted(input: String, dividers: List[Packet]): Seq[Packet] = {
    (input.split("\n\n")
      .flatMap {
        it =>
          val pair = it.split("\n").filter(_.nonEmpty)
          Seq(Packet(pair.head), Packet(pair.last))
      } ++ dividers).sorted
  }

  def position(packets: Seq[Packet], markers: Seq[Packet]): Seq[Int] = {
    markers.map(
      packet => packets.indexOf(packet) + 1
    )
  }


}
