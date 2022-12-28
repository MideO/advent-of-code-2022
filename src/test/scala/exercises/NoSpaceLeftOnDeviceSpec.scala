package com.github.mideo
package exercises

import com.github.mideo.exercises.ChangeDirectory.{Parent, Root}

import scala.collection.mutable
import scala.collection.mutable.Stack

class NoSpaceLeftOnDeviceSpec extends TestSpec {

  "SystemFileType" should {
    "create directory from input" in {
      SystemFileType("dir a") should be(Directory("a"))
    }

    "create file from input" in {
      SystemFileType("14848514 b.txt") should be(SystemFile("b.txt", 14848514L))
    }
    "Directory should determine size of all containing files" in {
      Directory("a",
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L),
          "c.txt" -> SystemFile("c.txt", 1000L),
          "d.txt" -> SystemFile("d.txt", 1000L))).size should be(3000L)
    }
    "Directory should determine size of containing files and directories" in {
      Directory("a",
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L),
          "x" -> Directory("x", mutable.Map("c.txt" -> SystemFile("c.txt", 1000L), "d.txt" -> SystemFile("d.txt", 1000L))),
          "e.txt" -> SystemFile("e.txt", 1000L))).size should be(4000L)
    }

    "SystemFile should determine size" in {
      SystemFile("b.txt", 14848514L).size should be(14848514L)
    }

  }
  "SystemFile " should {
    "have no children" in {
      SystemFile("b.txt", 1000L).children should be(Map.empty)
    }
  }
  "SystemFile Directory" should {
    "append children" in {
      val directory = Directory("a")
        .child(SystemFile("b.txt", 1000L))
        .child(SystemFile("c.txt", 1000L))
        .child(Directory("x", mutable.Map("y.txt" -> SystemFile("y.txt", 1000L), "z.txt" -> SystemFile("z.txt", 1000L))))
        .child(SystemFile("d.txt", 1000L))

      directory.children should be(
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L),
          "c.txt" -> SystemFile("c.txt", 1000L),
          "x" -> Directory("x", mutable.Map("y.txt" -> SystemFile("y.txt", 1000L), "z.txt" -> SystemFile("z.txt", 1000L))),
          "d.txt" -> SystemFile("d.txt", 1000L))
      )
      directory.size should be(5000L)
    }
    "list children" in {
      Directory("a",
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L),
          "c.txt" -> SystemFile("c.txt", 1000L),
          "x" -> Directory("x", mutable.Map("y.txt" -> SystemFile("y.txt", 1000L), "z.txt" -> SystemFile("z.txt", 1000L))),
          "d.txt" -> SystemFile("d.txt", 1000L))
      ).size should be(5000L)
    }
  }
  "FileSystemCommand" should {
    "change directory to root " in {
      val x = Directory("x", mutable.Map("y.txt" -> SystemFile("y.txt", 1000L), "z.txt" -> SystemFile("z.txt", 1000L)))
      val y = Directory("y", mutable.Map("i.txt" -> SystemFile("y.txt", 1000L), "j.txt" -> SystemFile("j.txt", 1000L)))
      val rootDirectory = Directory(
        Root,
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L),
          "c.txt" -> SystemFile("c.txt", 1000L),
          "x" -> x,
          "y" -> y,
          "d.txt" -> SystemFile("d.txt", 1000L))
      )
      val breadCrumb = Stack[SystemFileType](x, y, rootDirectory)
      ChangeDirectory(Root, rootDirectory, breadCrumb) should be(rootDirectory)
      breadCrumb.head should be(rootDirectory)
    }
    "change directory to sub directory " in {
      val x = Directory("x",
        mutable.Map(
          "y" -> Directory("y", mutable.Map("i.txt" -> SystemFile("y.txt", 1000L), "j.txt" -> SystemFile("j.txt", 1000L))),
          "z.txt" -> SystemFile("z.txt", 1000L))
      )
      val rootDir = Directory(
        Root,
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L),
          "c.txt" -> SystemFile("c.txt", 1000L),
          "x" -> x,
          "d.txt" -> SystemFile("d.txt", 1000L))
      )
      val breadCrumb = Stack[SystemFileType](rootDir)
      ChangeDirectory("x", rootDir, breadCrumb) should be(x)
      breadCrumb should be(Stack(x, rootDir))
    }
    "change multiple directory to sub directory " in {
      val y = Directory("y", mutable.Map("i.txt" -> SystemFile("y.txt", 1000L), "j.txt" -> SystemFile("j.txt", 1000L)))
      val x = Directory("x", mutable.Map("y" -> y, "z.txt" -> SystemFile("z.txt", 1000L)))
      val rootDir = Directory(
        Root,
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L), "c.txt" -> SystemFile("c.txt", 1000L), "x" -> x,
          "d.txt" -> SystemFile("d.txt", 1000L))
      )
      val breadCrumb = Stack[SystemFileType](x, rootDir)
      ChangeDirectory("y", rootDir, breadCrumb) should be(y)
      breadCrumb should be(Stack(y, x, rootDir))
    }
    "change directory to parent directory " in {
      val y = Directory("y", mutable.Map("i.txt" -> SystemFile("y.txt", 1000L), "j.txt" -> SystemFile("j.txt", 1000L)))
      val x = Directory("x", mutable.Map("y" -> y, "z.txt" -> SystemFile("z.txt", 1000L)))
      val rootDir = Directory(
        Root,
        mutable.Map("b.txt" -> SystemFile("b.txt", 1000L), "c.txt" -> SystemFile("c.txt", 1000L), "x" -> x,
          "d.txt" -> SystemFile("d.txt", 1000L))
      )
      val breadCrumb = Stack[SystemFileType](y, x, rootDir)
      ChangeDirectory(Parent, rootDir, breadCrumb) should be(x)
      breadCrumb should be(Stack(x, rootDir))
      ChangeDirectory(Parent, rootDir, breadCrumb) should be(rootDir)
      breadCrumb should be(Stack(rootDir))
    }
  }
  "FilesSystemDevice" should {
    val input =
      """
        |$ cd /
        |$ ls
        |dir a
        |14848514 b.txt
        |8504156 c.dat
        |dir d
        |$ cd a
        |$ ls
        |dir e
        |29116 f
        |2557 g
        |62596 h.lst
        |$ cd e
        |$ ls
        |584 i
        |$ cd ..
        |$ cd ..
        |$ cd d
        |$ ls
        |4060174 j
        |8033020 d.log
        |5626152 d.ext
        |7214296 k
        |""".stripMargin
    val fs = new FilesSystemDevice(input, 70000000L)
    "build filesystem" in {
      fs.rootDir.size should be (48381165L)
      fs.rootDir.children("d").size should be(24933642L)
      fs.rootDir.children("a").size should be(94853L)
      fs.rootDir.children("a").children("e").size should be(584L)
      fs.rootDir should be(
        Directory(
          Root,
          mutable.Map(
            "a" -> Directory(
              "a", mutable.Map(
                "e" -> Directory(
                  "e",
                  mutable.Map("i" -> SystemFile("i", 584L))
                ),
                "f" -> SystemFile("f", 29116L),
                "g" -> SystemFile("g", 2557L),
                "h.lst" -> SystemFile("h.lst", 62596L),

              ),
            ),
            "b.txt" -> SystemFile("b.txt", 14848514L),
            "c.dat" -> SystemFile("c.dat", 8504156L),
            "d" -> Directory("d", mutable.Map(
              "j" -> SystemFile("j", 4060174L),
              "d.log" -> SystemFile("d.log", 8033020L),
              "d.ext" -> SystemFile("d.ext", 5626152L),
              "k" -> SystemFile("k", 7214296L)
            ))
          )
        )
      )

    }
    "filter files" in {
      fs.findDirectory(d => d.size < 100000) should be(Seq(
        Directory(
        "a", mutable.Map(
          "e" -> Directory(
            "e",
            mutable.Map("i" -> SystemFile("i", 584L))
          ),
          "f" -> SystemFile("f", 29116L),
          "g" -> SystemFile("g", 2557L),
          "h.lst" -> SystemFile("h.lst", 62596L),

        )),
        Directory(
          "e",
          mutable.Map("i" -> SystemFile("i", 584L))
        )
      ))
    }
    "return unused space" in {
      fs.unusedSpace should be(21618835)
    }
    "return total deletion size required for update" in {
      fs.deletionSizeRequired(30000000L) should be(8381165L)
      fs.deletionSizeRequired(5000L) should be(0L)
    }
  }

  "NoSpaceLeftOnDevice" should {
    val input =
      """
        |$ cd /
        |$ ls
        |dir a
        |14848514 b.txt
        |8504156 c.dat
        |dir d
        |$ cd a
        |$ ls
        |dir e
        |29116 f
        |2557 g
        |62596 h.lst
        |$ cd e
        |$ ls
        |584 i
        |$ cd ..
        |$ cd ..
        |$ cd d
        |$ ls
        |4060174 j
        |8033020 d.log
        |5626152 d.ext
        |7214296 k
        |""".stripMargin
    "return sum of all directories matching given criteria" in {
      NoSpaceLeftOnDevice.sum(input, d => d.size < 100000) should be(95437L)
    }

    "return size of smallest file delete to perform Update" in {
      NoSpaceLeftOnDevice.findSmallestDirectorySizeToDeleteToPerformUpdate(input, 30000000L) should be(24933642)
    }
  }
}
