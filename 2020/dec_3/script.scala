#!/usr/bin/env amm

import scala.io.Source

val raw_data = Source.fromFile("input.txt").mkString

object Forest {

  def apply(string: String): Forest = {
    val grid: Array[List[Char]] = string.split("\n").map(_.toList)
    val width: Int = grid(0).length

    Forest(grid, grid.length, width)
  }
}

case class Forest(grid: Array[List[Char]], height: Int, width: Int) {
  def tree(x: Int, y: Int): Boolean = {
    grid(y)(x % width) == '#'
  }
}

def trees_on_grad_3(f: Forest): Int = {
  (0 until f.height).count(i => f.tree(3*i, i))
}

val f = Forest(raw_data)

printf("%d\n", trees_on_grad_3(f))

case class Grad(run: Int, fall: Int)

def trees_on_grad(f: Forest, grad: Grad): Int = {
  (0 until (f.height) by grad.fall).count(i => f.tree(grad.run*i, i))
}

print(List(
  Grad(1, 1),
  Grad(3, 1),
  Grad(5, 1),
  Grad(7, 1),
  Grad(1, 2),
).map(g => trees_on_grad(f, g))
.foldLeft(BigInt(1)){
  (prod, i) => prod * i
})
