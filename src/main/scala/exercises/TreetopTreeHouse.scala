package com.github.mideo
package exercises

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

// https://adventofcode.com/2022/day/8
case class Tree(height: Int, row: Int, column: Int)

case class MapOfTrees(area: Seq[Seq[Tree]])

object Quadcopter {
  def scan(input: String): MapOfTrees = MapOfTrees(
    iterator(input)
      .zipWithIndex
      .map(
        line => line._1
          .toCharArray
          .zipWithIndex
          .map(column => Tree(column._1.asDigit, line._2, column._2)).toSeq
      ).toSeq)
}

object TreetopTreeHouse {
  private val rowSelector: (Tree, Int, Seq[Seq[Tree]]) => Tree = (tree, x, a) => a(x)(tree.column)
  private val columnSelector: (Tree, Int, Seq[Seq[Tree]]) => Tree = (tree, x, a) => a(tree.row)(x)

  def visibleTreesFromBoarder(input: String): Seq[Tree] = {
    val map = Quadcopter.scan(input)
    map.area
      .flatMap(trees => trees.filter(tree => isVisible(tree, map)))
  }

  def highestScenicScore(input: String): (Tree, Int) = {
    val map = Quadcopter.scan(input)
    val scores = map.area
      .flatMap(trees => trees.map(tree => (tree, scenicScore(tree, map))))
      .sorted((x: (Tree, Int), y: (Tree, Int)) => y._2.compareTo(x._2))
    scores.head
  }

  private def scenicScore(tree: Tree, map: MapOfTrees): Int = {
    val rowEnd = map.area.length
    val columnEnd = map.area.head.length
    count(tree, map.area, (0 until tree.row).reverse, rowSelector) *
      count(tree, map.area, tree.row+1  until  rowEnd, rowSelector) *
      count(tree, map.area, (0 until tree.column).reverse, columnSelector) *
      count(tree, map.area, (tree.column+1  until columnEnd), columnSelector)
  }

  private def count(tree: Tree, area: Seq[Seq[Tree]], range: Range, selector: (Tree, Int, Seq[Seq[Tree]]) => Tree): Int = {
    val counter = new mutable.Stack[Tree]()
    breakable {
      range.foreach {
        case x if selector(tree, x, area).height >= tree.height =>
          counter.push(selector(tree, x, area))
          break()
        case x => counter.push(selector(tree, x, area))
      }
    }
    counter.size
  }

  private def isVisible(tree: Tree, map: MapOfTrees): Boolean = {
    val rowEnd = map.area.length - 1
    val columnEnd = map.area.head.length - 1
    if (tree.row == 0 || tree.column == 0 || tree.row == rowEnd || tree.column == columnEnd) true
    else {
      visible(tree, map.area, 0 to tree.row, rowSelector) ||
        visible(tree, map.area, (tree.row to rowEnd).reverse, rowSelector) ||
        visible(tree, map.area, 0 to tree.column, columnSelector) ||
        visible(tree, map.area, (tree.column to columnEnd).reverse, columnSelector)
    }
  }

  private def visible(tree: Tree, area: Seq[Seq[Tree]], range: Range, selector: (Tree, Int, Seq[Seq[Tree]]) => Tree): Boolean = {
    range.map(x => selector(tree, x, area))
      .reduce((x, y) => if (x.height < y.height) y else x).equals(tree)
  }
}