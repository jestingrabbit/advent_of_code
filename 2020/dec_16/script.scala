#!/usr/bin/env amm

import scala.io.Source

val Array(rulesStr, yourTicketStr, nearbyTicketStr) = Source.fromFile("input.txt").mkString.split("\n\n")

object Rule {
  def apply(str: String): Rule = {
    val Array(name, boundsStr) = str.split(": ")
    val bounds = boundsStr.split(" or ").map(Bound(_))
    new Rule(name, bounds)
  }
}

class Rule(val name: String, val bounds: Array[Bound]) {
  def accepts(i: Int): Boolean = bounds.exists(b => b.accepts(i))
}

object Bound {
  def apply(str: String): Bound = {
    val Array(low, high) = str.split("-").map(_.toInt)
    Bound(low, high)
  }
}

case class Bound(val low: Int, val high: Int) {
  def accepts(i: Int): Boolean = !(i < low || i > high)
}

def ingestRules(str: String):(Bound, List[Rule]) = {
  str.split("\n").foldLeft((Bound(1000, 0), List[Rule]()))((agg, row) => {
    val (bound, rs) = agg
    val newRule = Rule(row)
    (Bound(bound.low.min(newRule.bounds(0).low), bound.high.max(newRule.bounds(1).high)), newRule :: rs)
  })
}

val (extremes, rulesList) = ingestRules(rulesStr)

val tickets = nearbyTicketStr.split("\n").drop(1).map(r => {
   r.split(",").map(_.toInt)
})

val scanningError = tickets.map(t => t.filter(i => !extremes.accepts(i)).sum).sum

println(scanningError)

val soundTickets = tickets.filter(t => t.forall(i => extremes.accepts(i)))

println(soundTickets.length)

val rules = rulesList.map(r => (r.name -> r)).toMap

val ruleNames = rules.keySet

val myTicket = yourTicketStr.split("\n")(1).split(",").map(_.toInt)

val possibleRules = (0 to 19).map(i => {
  ruleNames.filter(n => rules(n).accepts(myTicket(i)))
}).toArray

def iterativeReducer(tup: (Array[Set[String]], Map[String, Int])):(Array[Set[String]], Map[String, Int]) = {
  val newTup = (0 to 19).foldLeft(tup)((aggN, i) => {
    val (rs, ss) = aggN
    if (rs(i).size == 1) {
      val solved = rs(i).head
      (rs.map(_ - solved), ss + (solved -> i))
    } else {
      aggN
    }
  })

  if (newTup._1.map(_.size).sum < tup._1.map(_.size).sum) {
    iterativeReducer(newTup)
  } else {
    newTup
  }
}

val ruleCorrespondenceNearly = soundTickets.foldLeft((possibleRules, Map[String, Int]()))((agg, t) => {
  val (prs, sols) = agg
  val newPossibles = (0 to 19).map(i => {
    prs(i).filter(n => rules(n).accepts(t(i)))
  }).toArray
  iterativeReducer((newPossibles, sols))
})

val ruleCorrespondence = ruleCorrespondenceNearly._2

ruleCorrespondence.foreach(println(_))

println(ruleNames.filter(n => n.take(9) == "departure").map(n => myTicket(ruleCorrespondence(n)).toLong).reduce(_ * _))
