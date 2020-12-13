#!/usr/bin/env amm

import scala.io.Source
import scala.collection.immutable.Queue

val rawData = Source.fromFile("input.txt").mkString

object SeatingArrangement {
  def apply(str: String): SeatingArrangement = {
    val rows = str.split("\n")
    val height = rows.length
    val width = rows(0).length
    val grid = rows.reduce(_ ++ _).toArray
    val seats = (for {
      i <- 0 until width
      j <- 0 until height
      if grid(i + j * width) != '.'
    } yield (i,j)).toSet
    new SeatingArrangement(grid, width, height, seats)
  }
}

class SeatingArrangement(val grid: Array[Char], val width: Int, val height: Int, val changed: Set[Tuple2[Int, Int]]) {
  def apply(x: Int, y: Int): Char = grid(y * width + x)
  def apply(pr: Tuple2[Int, Int]): Char = grid(pr._2 * width + pr._1)
  def inBounds(x: Int, y: Int): Boolean = (x >= 0 && x < width) && (y >= 0 && y < height)
  def neighbourSeats(x: Int, y: Int): Set[Tuple2[Int, Int]] = {
    val r = (-1 to 1)
    (for {
      i <- r
      j <- r
      xNew = i + x
      yNew = j + y
      if (i != 0 || j != 0) && inBounds(xNew, yNew) && grid(xNew + yNew * width) != '.'
    } yield (xNew, yNew)).toSet
  }
  def neighbourSeats(pr: Tuple2[Int, Int]): Set[Tuple2[Int, Int]] = neighbourSeats(pr._1, pr._2)
  def occupiedNeighbours(x: Int, y: Int): Int = {
    neighbourSeats(x, y).count(apply(_) == '#')
  }
  def occupiedNeighbours(pr: Tuple2[Int, Int]): Int = occupiedNeighbours(pr._1, pr._2)
  def step: SeatingArrangement = {
    val changers = changed.flatMap(neighbourSeats).union(changed).filter(pr =>{
      val n = occupiedNeighbours(pr)
      val g = apply(pr)
      (g == '#' && n >= 4) || (g == 'L' && n == 0)
    })
    val newGrid = grid.zipWithIndex.map(pr => {
      val (g, i) = pr
      val y = i / width
      val x = i - y * width
      if (changers.contains((x, y))) {
        if (g == '#') 'L' else '#'
      } else g
    })

    new SeatingArrangement(newGrid, width, height, changers)
  }
  def sees(x: Int, y: Int): Set[Tuple2[Int, Int]] = {
    val r = (-1 to 1)
    val vs: Array[Tuple2[Int,Int]] = (for {
      i <- r
      j <- r
      if (i != 0 || j != 0)
    } yield (i, j)).toArray
    val ms = vs.map(a => (a , (x, y))).toMap
    sees(ms, Set[(Int, Int)]())
  }
  def sees(ms: Map[(Int,Int),(Int,Int)], seen: Set[(Int,Int)]): Set[(Int,Int)] = {
    if (ms.isEmpty) seen
    else {
      val base: (Map[(Int, Int), (Int, Int)], Set[(Int, Int)]) = (Map(), seen)
      val results = ms.foldLeft(base)((agg, pr) => {
        val (msn, seenn) = agg
        pr match {
          case ((dx, dy), (x, y)) => {
            val xN = x + dx
            val yN = y + dy
            val ptN = (xN, yN)
            if (inBounds(xN, yN))
              if (apply(xN, yN) == '.'){
                (msn + ((dx, dy) -> ptN), seenn)
              }

              else
                (msn, (seenn + ptN))
            else
              agg
          }
        }
      })
      sees(results._1, results._2)
    }
  }

  def visions: Map[Tuple2[Int, Int], Set[Tuple2[Int, Int]]] = {
    changed.map(a => (a, sees(a._1, a._2))).toMap
  }

  def sightStep(vis: Map[Tuple2[Int, Int], Set[Tuple2[Int, Int]]]): SeatingArrangement = {
    val changers = changed.flatMap(vis.getOrElse(_, Set())).union(changed).filter(pr =>{
      val n = vis.getOrElse(pr, Set()).count(apply(_) == '#')
      val g = apply(pr)
      (g == '#' && n >= 5) || (g == 'L' && n == 0)
    })
    val newGrid = grid.zipWithIndex.map(pr => {
      val (g, i) = pr
      val y = i / width
      val x = i - y * width
      if (changers.contains((x, y))) {
        if (g == '#') 'L' else '#'
      } else g
    })

    new SeatingArrangement(newGrid, width, height, changers)
  }
}

val start = SeatingArrangement(rawData)

var arrs = List(start)

while (! arrs.head.changed.isEmpty) {
  arrs = arrs.head.step :: arrs
}

println(arrs.head.grid.count(_ == '#'))

val visions = start.visions

var arrs2 = List(start)

while (! arrs2.head.changed.isEmpty) {
  arrs2 = arrs2.head.sightStep(visions) :: arrs
}

println(arrs2.head.grid.count(_ == '#'))
