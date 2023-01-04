package com.github.mideo
package exercises

import exercises.Direction.{R, U, _}
import inputs.RopeBridgeInput

class RopeBridgeSpec extends TestSpec {

  "Knot" should {
    "have initial position" in {
      new Knot().position should be(Position(0, 0))
      new Knot().position should be(Position(0, 0))
    }


    "move to right" in {
      val knot = new Knot()
      Repeat(4) { _ => knot.move(R) }
      knot.position should be(Position(4, 0))
    }

    "move to left" in {
      val knot = new Knot()
      Repeat(4) { _ => knot.move(R) }
      knot.position should be(Position(4, 0))
      Repeat(2) { _ => knot.move(L) }
      knot.position should be(Position(2, 0))
    }
    "move  up" in {
      val knot = new Knot()
      Repeat(4) { _ => knot.move(U) }
      knot.position should be(Position(0, 4))
    }

    "move down" in {
      val knot = new Knot()
      Repeat(4) { _ => knot.move(U) }
      knot.position should be(Position(0, 4))
      Repeat(3) { _ => knot.move(D) }
      knot.position should be(Position(0, 1))
    }

    "Head keep visited count" in {
      val knot = new Knot()

      Repeat(4) { _ => knot.move(R) }
      Repeat(3) { _ => knot.move(U) }
      Repeat(1) { _ => knot.move(D) }
      Repeat(4) { _ => knot.move(L) }

      knot.visited.size should be(12)
      knot.position should be(Position(0, 2))
    }

    "Tail unique visited count" in {
      val knot = new Knot()
      Repeat(4) { _ => knot.move(R) }
      Repeat(3) { _ => knot.move(U) }
      Repeat(1) { _ => knot.move(D) }
      Repeat(4) { _ => knot.move(L) }
      knot.visited.size should be(12)
      knot.position should be(Position(0, 2))
    }
    "Tail move right towards head when distance is two spaces" in {
      //......
      //......
      //......
      //......
      //T.H... (0,0),(2,0)
      val head = new Knot()
      val tail = new Knot()
      Repeat(2) { _ => head.move(R) }
      tail.moveCloser(head)
      tail.position should be(Position(1, 0))
    }
    "Tail not move right towards head when distance is one space" in {
      //......
      //......
      //......
      //......
      //HT.... (0,0),(1,0)
      val head = new Knot()
      val tail = new Knot()
      head.move(R)
      tail.moveCloser(head)
      tail.position should be(Position(0, 0))
    }
    "Tail move left towards head when distance is two spaces" in {
      //......
      //......
      //......
      //......
      //...H.T (3,0),(5,0)
      val head = new Knot()
      val tail = new Knot()
      Repeat(3) { _ => head.move(R) }
      Repeat(5) { _ => tail.move(R) }
      tail.moveCloser(head)
      tail.position should be(Position(4, 0))
    }
    "Tail not move left towards head when distance is one space" in {
      //......
      //......
      //......
      //......
      //...HT. (3,0),(4,0)
      val head = new Knot()
      val tail = new Knot()
      Repeat(3) { _ => head.move(R) }
      Repeat(4) { _ => tail.move(R) }
      tail.moveCloser(head)
      tail.position should be(Position(4, 0))
    }

    "Tail up right towards head when distance is two spaces" in {
      //......
      //......
      //H.....(0,2)
      //......
      //T..... (0,0)
      val head = new Knot()
      val tail = new Knot()
      Repeat(2) { _ => head.move(U) }
      tail.moveCloser(head)
      tail.position should be(Position(0, 1))
    }
    "Tail not up right towards head when distance is one space" in {
      //......
      //......
      //......
      //H.....(0,1)
      //T..... (0,0)
      val head = new Knot()
      val tail = new Knot()
      head.move(U)
      tail.moveCloser(head)
      tail.position should be(Position(0, 0))
    }

    "Tail up down towards head when distance is two spaces" in {
      //......
      //......
      //T..... (0,2)
      //......
      //H.....(0,0)
      val head = new Knot()
      val tail = new Knot()
      Repeat(2) { _ => tail.move(U) }
      tail.moveCloser(head)
      tail.position should be(Position(0, 1))
    }
    "Tail not down right towards head when distance is one space" in {
      //......
      //......
      //......
      //T.....(0,1)
      //H.....(0,0)
      val head = new Knot()
      val tail = new Knot()
      tail.move(U)
      tail.moveCloser(head)
      tail.position should be(Position(0, 1))
    }

    "Tail not up right towards head when x distance is one space and y distance is one space" in {
      //......
      //......
      //......
      //.H....(1,1)
      //T.....(0,1)
      val head = new Knot()
      val tail = new Knot()
      head.move(U)
      head.move(R)
      tail.moveCloser(head)
      tail.position should be(Position(0, 0))
    }
    "Tail should move up and right towards head when x distance is one space and y distance is two spaces" in {
      //......
      //......
      //..H...(2,2)
      //......
      //.T....(1,0)
      val head = new Knot(StartingPosition = Position(2,2))
      val tail = new Knot(StartingPosition = Position(1,0))
      tail.moveCloser(head)
      tail.position should be(Position(2, 1))

    }
    "Tail should move up and left towards head when x distance is one space and y distance is two spaces" in {
      //......
      //......
      //H..... (0,2)
      //......
      //.T.... (1,0)
      val head = new Knot(StartingPosition = Position(0,2))
      val tail = new Knot(StartingPosition = Position(1,0))
      tail.moveCloser(head)
      tail.position should be(Position(0, 1))
    }
    "Tail should down  and right towards head when x distance is two spaces and y distance is two spaces" in {
      //......
      //......
      //T..... (0,2)
      //......
      //.H.... (1,0)
      val head = new Knot(StartingPosition = Position(1,0))
      val tail = new Knot(StartingPosition = Position(0,2))
      tail.moveCloser(head)
      tail.position should be(Position(1, 1))
    }

    "Tail should down and left towards head when x distance is two spaces and y distance is two spaces" in {
      //......
      //......
      //..T... (2,2)
      //......
      //.H.... (1,0)
      val head = new Knot(StartingPosition = Position(1,0))
      val tail = new Knot(StartingPosition = Position(2,2))
      tail.moveCloser(head)
      tail.position should be(Position(1, 1))
    }


    "Tail should move up and left towards head when x distance is two space and y distance is one spaces" in {
      //......
      //......
      //......
      //H..... (0,1)
      //..T... (2,0)
      val head = new Knot(StartingPosition = Position(0,1))
      val tail = new Knot(StartingPosition = Position(2,0))
      tail.moveCloser(head)
      tail.position should be(Position(1, 1))
    }

    "Tail should move up and right towards head when x distance is two space and y distance is one spaces" in {
      //......
      //......
      //......
      //..H... (2,1)
      //T..... (0,0)
      val head = new Knot(StartingPosition = Position(2,1))
      val tail = new Knot(StartingPosition = Position(0,0))


      tail.moveCloser(head)
      tail.position should be(Position(1, 1))
    }


    "Tail should move down and left towards head when x distance is two space and y distance is one spaces" in {
      //......
      //......
      //......
      //..T... (2,1)
      //H..... (0,0)
      val head = new Knot(StartingPosition = Position(0,0))
      val tail = new Knot(StartingPosition = Position(2,1))


      tail.moveCloser(head)
      tail.position should be(Position(1, 0))
    }
    "Tail should move down and right towards head when x distance is two space and y distance is one spaces" in {
      //......
      //......
      //......
      //..T... (2,1)
      //....H. (4,0)
      val head = new Knot(StartingPosition = Position(4,0))
      val tail = new Knot(StartingPosition = Position(2,1))

      tail.moveCloser(head)
      tail.position should be(Position(3, 0))
    }

    "Tail not should move down and right towards head when x distance is one space and y distance is one spaces in negative  axis" in {
      //......
      //.....s
      //......
      //...T.. (-2,-2)
      //..H... (-3,-4)
      val head = new Knot(StartingPosition = Position(-3,-3))
      val tail = new Knot(StartingPosition = Position(-2,-2))

      tail.moveCloser(head)
      tail.position should be(Position(-2, -2))
    }
    "Tail 2 should move closer" in {
      //......
      //...H..
      //......
      //.21... (-2,-2)
      //s..... (-3,-4)
      val head = new Knot(StartingPosition = Position(3, 3))
      val tail = new Knot(StartingPosition = Position(2, 1))
      val tail2 = new Knot(StartingPosition = Position(1, 1))

      tail.moveCloser(head)
      tail.position should be(Position(3, 2))
      tail2.moveCloser(tail)
      tail2.position should be(Position(2, 2))
    }

    "Tail not should move down and right towards head when x distance is two space and y distance is one spaces in negative  axis" in {
      //......
      //.....s
      //......
      //...T.. (-2,-2)
      //......
      //..H... (-3,-4)
      val head = new Knot(StartingPosition = Position(-3, -4))
      val tail = new Knot(StartingPosition = Position(-2, -2))

      tail.moveCloser(head)
      tail.position should be(Position(-3, -3))
    }

  }

  "RopeBridge" should {
    "record tail visit" in {
      val input =
        """
          |R 4
          |U 4
          |L 3
          |D 1
          |R 4
          |D 1
          |L 5
          |R 2
          |""".stripMargin

      val knots = RopeBridge.processMovement(input, new Knot(1), new Knot(2))
      knots.last.visited.size should be(13)

    }

    "record tail visit - small" in {
      val input =
        """
          |R 4
          |U 4
          |L 3
          |D 1
          |R 4
          |D 1
          |L 5
          |R 2
          |""".stripMargin

      val knots = RopeBridge.processMovement(input,(0 to 9).map(n => new Knot(n)):_*)
      knots.last.visited.size should be(1)

    }

    "record multiple knots tail visit" in {
      val input =
        """
          |R 5
          |U 8
          |L 8
          |D 3
          |R 17
          |D 10
          |L 25
          |U 20
          |""".stripMargin

      val knots = RopeBridge.processMovement(input, (0 to 9).map(n => new Knot(n)):_*)
      knots.last.visited.size should be(36)
    }

  }

}
