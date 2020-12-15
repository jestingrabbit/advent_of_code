#!/usr/bin/env amm

import scala.io.Source

val initialNumbers = Source.fromFile("input.txt").mkString.split("\n")(0).split(",").map(_.toInt)

object Game {
  def apply(arr: Array[Int]): Game = {
    val lastSeen = arr.zip((1 to arr.length)).toMap
    val lastSaid = arr.last
    val turn = arr.length
    new Game(turn, lastSaid, lastSeen)
  }
}

class Game(val turn: Int, val lastSaid: Int, val lastSeen: Map[Int, Int]) {
  def step: Game = {
    val lastSeenLastSaid = lastSeen.getOrElse(lastSaid, 0)
    val say =
      if (lastSeenLastSaid == 0)
        0
      else
        turn - lastSeenLastSaid
    new Game(turn + 1, say, lastSeen + (lastSaid -> turn))
  }
}

val begins = Game(initialNumbers)

println(((begins.turn + 1) to 2020).foldLeft(begins)((g, _) => g.step).lastSaid)
println(((begins.turn + 1) to 30000000).foldLeft(begins)((g, _) => g.step).lastSaid)
