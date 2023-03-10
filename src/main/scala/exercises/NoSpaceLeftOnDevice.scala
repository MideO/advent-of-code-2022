package com.github.mideo
package exercises


import scala.collection.mutable
import scala.collection.mutable.Stack

// https://adventofcode.com/2022/day/7
object ChangeDirectory {
  val Root = "/"
  val Parent = ".."

  def apply(name: String, rootFile: SystemFileType, breadCrumb: Stack[SystemFileType]): SystemFileType = {
    name match {
      case _ if Parent.equals(name) =>
        breadCrumb.pop()
        breadCrumb.head
      case _ if Root.equals(name) =>
        breadCrumb.clear()
        breadCrumb.push(rootFile).head
      case x => breadCrumb.push(breadCrumb.head.children(x)).head
    }
  }
}

sealed trait SystemFileType {
  val name: String

  def size: Long

  val children: mutable.Map[String, SystemFileType] = mutable.Map[String, SystemFileType]()

  def filter(criteria: SystemFileType => Boolean): Seq[SystemFileType] = if (criteria.apply(this)) Seq(this) else Seq.empty
}

case class Directory(name: String, override val children: mutable.Map[String, SystemFileType] = mutable.Map[String, SystemFileType]()) extends SystemFileType {
  def child(file: SystemFileType): Directory = {
    children(file.name) = file
    this
  }

  override def size: Long = children.map(_._2.size).sum

  override def filter(criteria: SystemFileType => Boolean): Seq[SystemFileType] = super.filter(criteria) ++ children.flatMap(it => it._2.filter(criteria)).toSeq
}


case class SystemFile(name: String, fileSize: Long) extends SystemFileType {
  override def size: Long = fileSize
}

object SystemFileType {
  def apply(input: String): SystemFileType = {
    val split = input.split(" ")
    if (split.head.equals("dir"))
      Directory(split.last)
    else
      SystemFile(split.last, split.head.toLong)
  }
}

class FilesSystemDevice(input: String, totalDiskSpace: Long = 70000000L) {

  def rootDir: Directory = breadCrumb.last.asInstanceOf[Directory]

  def findDirectory(filter: SystemFileType => Boolean): Seq[SystemFileType] = rootDir.filter(d => filter(d) && d.isInstanceOf[Directory])

  def unusedSpace: Long = totalDiskSpace - rootDir.size

  def deletionSizeRequired(updateSize: Long): Long = if (unusedSpace > updateSize) 0L else updateSize - unusedSpace


  private val breadCrumb: Stack[SystemFileType] = Stack(Directory("/"))
  iterator(input).foreach {
    case x if x.startsWith("$ cd") => ChangeDirectory(x.split(" ").last, rootDir, breadCrumb)
    case x if x.startsWith("$ ls") => breadCrumb.head
    case x => breadCrumb.head.asInstanceOf[Directory].child(SystemFileType(x))
  }

}

object NoSpaceLeftOnDevice {
  def sum(input: String, filter: SystemFileType => Boolean, totalDiskSpace: Long = 70000000L): Long = {
    new FilesSystemDevice(input, totalDiskSpace)
      .findDirectory(filter)
      .map(_.size).sum
  }

  def findSmallestDirectorySizeToDeleteToPerformUpdate(input: String, updateSize: Long, totalDiskSpace: Long = 70000000L): Long = {
    val fs  = new FilesSystemDevice(input, totalDiskSpace)
    val candidates  = fs.findDirectory(d => d.size > fs.deletionSizeRequired(updateSize))
    candidates.min[SystemFileType]((x, y) => x.size.compare(y.size)).size
  }
}

