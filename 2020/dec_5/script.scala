#!/usr/bin/env amm

import scala.io.Source

val raw_data = Source.fromFile("input.txt").mkString

object Seat {
  val binMap = Map('F' -> 0, 'B' -> 1, 'L' -> 0, 'R' -> 1)

  def toInt(str: String): Int = {
    str.toList.map(binMap).foldLeft(0)((a, i) => 2*a + i)
  }
}

case class Seat(pass: String) {
  def id = Seat.toInt(pass)
}

val seats = raw_data.split("\n").map(Seat(_))

val ids = seats.map(_.id)

println(ids.max)

println(ids.sorted.sliding(2).find(
  ar => ar match {
    case Array(a, b) => (b - a == 2)
    case _ => false
  }) match {
    case Some(ar) => ar(0) + 1
    case _ => -1
  }
)
