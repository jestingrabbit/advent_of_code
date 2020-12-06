#!/usr/bin/env amm

import scala.io.Source

val rawData = Source.fromFile("input.txt").mkString

println(rawData.split("\n\n").map(s => s.toSet.-('\n').size).sum)

val alphabet = ('a' to 'z').toSet

println(rawData.split("\n\n").map(s =>
  s.split("\n").map(_.toSet).foldLeft(alphabet)((a, s) => a & s).size).sum)
