#!/usr/bin/env amm

import scala.io.Source

val rawData = Source.fromFile("input.txt").mkString

case class Voxel(x: Int, y: Int, z: Int) {
  def neighbours: Set[Voxel] = {
    val r = (-1 to 1)
    (for {
      i <- r; j <- r; k <- r
    } yield Voxel(x + i, y + j, z + k)).toSet - this
  }
}

object Grid {
  def apply(str: String): Grid = {
    val s = str.split("\n").zipWithIndex.flatMap(pr => {
      val (r, i) = pr
      r.toArray.zipWithIndex.filter(_._1 == '#').map(ppr => {
        val (_, j) = ppr
        Voxel(i, j, 0)
      }).toSet
    })
    new Grid(s)
  }
}

class Grid(val voxels: Set[Voxel]) {
  def step: Grid = {
    voxels.map(_.neighbours).union(voxels)
  }
}
