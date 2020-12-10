#!/usr/bin/env amm

import scala.io.Source
import scala.collection.immutable.Queue

val sortedNumbers = Source.fromFile("input.txt").getLines.map(_.toInt).toList.sorted

val initial: Map[Int, Int] = if (sortedNumbers.head == 3) {
  Map(3 -> 2)
} else {
  Map(sortedNumbers.head -> 1, 3 -> 1)
}

val gaps = sortedNumbers.sliding(2).foldLeft(initial)((a, ls) => {
  val diff = ls.tail.head - ls.head
  a + (diff -> (a.getOrElse(diff, 0) + 1))
})

println(gaps)
println(gaps.getOrElse(1, 0) * gaps.getOrElse(3, 0))

val start = List((0, 1L))

val waysToCharge = sortedNumbers.foldLeft(start)((a, i) => {
  val reachable = a.filter(i - _._1 <= 3)
  val waysToReach = reachable.map(_._2).sum
  (i, waysToReach) :: reachable
})

println(waysToCharge.head._2)
