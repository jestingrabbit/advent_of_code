#!/usr/bin/env amm

import scala.io.Source
import scala.math

val lines = Source.fromFile("input.txt").mkString.split("\n")

val readyAt = lines(0).toInt

val buses = lines(1).split(",").filter(_ != "x").map(_.toInt)

val (waitTime, bus) = buses.map(b => (b - (readyAt % b), b)).min

println(waitTime * bus)

val busesAndOffsets = lines(1).split(",").zipWithIndex.filter(_._1 != "x").map(pr => {
  val i = pr._1.toInt
  (i, (i - (pr._2 % i)) % i)
})

busesAndOffsets.foreach(println(_))

val answer2 = busesAndOffsets.foldLeft((1L, 0L))((pr1, pr2) => {
  val (b1, m1) = pr1
  val (b2, m2) = pr2
  println(pr1)
  // (0 until b2).foreach(a => println((a * b1 + m1) % b2))
  ((b1*b2).toLong, (0 until b2).map(_ * b1 + m1).find(_ % b2 == m2).get)
})

println(answer2._2)
