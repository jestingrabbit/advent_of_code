#!/usr/bin/env amm

import scala.io.Source
import scala.collection.immutable.Queue

def numbers: Iterator[Long] = {
  Source.fromFile("input.txt").getLines.map(_.toLong)
}

class XmasState(val vals: Queue[Long], val sums: Map[Long, Long]) {
  def push(i: Long): XmasState = {
    val newSums = vals.map(_ + i).foldLeft(sums)((a, s) =>
      a + (s -> (a.getOrElse(s, 0L) + 1L)))
    new XmasState(vals.enqueue(i), newSums)
  }

  def valid(i: Long): Boolean = sums.contains(i)

  def pop: XmasState = {
    val (i, newVals) = vals.dequeue
    val newSums = newVals.map(_ + i).foldLeft(sums)((a, s) => {
      val i = a.getOrElse(s, 1L)
      if (i == 1L) {
        a - i
      } else {
        a + (s -> (i - 1L))
      }
    })
    new XmasState(newVals, newSums)
  }

  def insert(i: Long): XmasState = {
    val pushable = if (vals.length == XmasState.size) {
      this.pop
    } else {
      this
    }

    pushable.push(i)
  }
}

object XmasState {
  val size: Long = 25

  def empty: XmasState = new XmasState(Queue[Long](), Map[Long, Long]())
}

def findInvalid(numbers: Iterator[Long], state: XmasState): Long = {
  val i = numbers.next
  if (state.vals.length < XmasState.size || state.valid(i)) {
    findInvalid(numbers, state.insert(i))
  } else {
    i
  }
}

val target = findInvalid(numbers, XmasState.empty)

println(target)

def findContiguousSummers(numbers: Iterator[Long], vals: Queue[Long], sum: Long, target: Long): Long = {
  if (sum == target) {
    vals.min + vals.max
  } else if (sum < target) {
    val i = numbers.next
    findContiguousSummers(numbers, vals.enqueue(i), sum + i, target)
  } else {
    val (i, newVals) = vals.dequeue
    findContiguousSummers(numbers, newVals, sum - i, target)
  }
}

println(findContiguousSummers(numbers, Queue(), 0, target))
