#!/usr/bin/env amm

import scala.io.Source

val inputSrc = Source.fromFile("input.txt")

val lines = inputSrc.getLines

case class Bag(adj: String, color: String)

object Bag {
  def apply(str: String): Bag = {
    str.split(" ") match {
      case Array(adj, color, _*) => Bag(adj, color)
    }
  }

  def apply(arr: Array[String]): Bag = Bag(arr(0), arr(1))
}

class Rule(val container: Bag, val contents: Map[Bag, Int])

val start: Tuple2[Map[Bag, Rule], Map[Bag, Set[Bag]]] = (Map(),Map())

def injest(lines: Iterator[String]): Tuple2[Map[Bag, Rule], Map[Bag, Set[Bag]]] = {
  lines.foldLeft(start)( (agg, line) => {
    val (bagsToRules, bagsToContainers) = agg
    line.split(" contain ") match {
      case Array(containerStr, contentsStr, _*) => {
        val container = Bag(containerStr)
        val contents = contentsStr.take(3) match {
          case "no " => Map[Bag, Int]()
          case _ => contentsStr.split(", ").map(s => {
            val parts = s.split(" ")
            (Bag(parts.drop(1)), parts(0).toInt)
          }).toMap
        }

        (
          bagsToRules + (container -> new Rule(container, contents)),
          contents.toList.foldLeft(bagsToContainers)((bToCs, tup) => {
            val (bag, _) = tup
            val v = bToCs.getOrElse(bag, Set())
            bToCs.updated(bag, v + container)
          })
        )
      }
    }
  })
}

val (bagsToRules, bagsToContainers) = injest(lines)

def everythingThatContains(seenBags: Set[Bag], newBags: Set[Bag]): Set[Bag] = {
  val seenNow = seenBags ++ newBags
  val furtherContainers = newBags.flatMap(b => bagsToContainers.getOrElse(b, Set())) -- seenNow
  if (furtherContainers.isEmpty) {
    seenNow
  } else {
    everythingThatContains(seenNow, furtherContainers)
  }
}

println(everythingThatContains(Set(), Set(Bag("shiny", "gold"))).size)

def contains(bag: Bag): Int = {
  bagsToRules(bag).contents.map( pr => {
    pr._2 * (1 + contains(pr._1))
  }).sum
}

println(contains(Bag("shiny", "gold")))

println("woo")
