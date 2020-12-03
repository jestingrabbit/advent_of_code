#!/usr/bin/env amm

import scala.io.Source

val numbers = Source
  .fromFile("input.txt")
  .mkString.split("\n")
  .map(_.toInt)

def find_product(ls: Seq[Int], ss: Set[Int]): Int = {
  ls match {
    case hd +: rest => if (ss.contains(2020 - hd)) {
      hd * (2020 - hd)
    } else {
      find_product(rest, ss + hd)
    }
    case _ => 0
  }
}

printf("%d\n", find_product(numbers, Set()))

def find_triple_product(ls: Seq[Int], seen: Set[Int], pairs: Set[Tuple3[Int,Int,Int]]): Int = {
  ls match {
    case hd +: rest => {
      val complement : Option[Tuple3[Int,Int,Int]] = pairs.find(t3 => (hd + t3._1 == 2020))
      complement match {
        case Some(ts) => hd * ts._2 * ts._3
        case None => find_triple_product(rest, seen + hd, pairs ++ seen.map(s => (s + hd, s, hd)))
      }
    }
    case _ => 0
  }
}

printf("%d\n", find_triple_product(numbers, Set(), Set()))
