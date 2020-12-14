#!/usr/bin/env amm

import scala.io.Source

val lines = Source.fromFile("input.txt").getLines

object Inst {
  def apply(str: String): Inst = {
    val as = str.split(" = ")
    if (as(0) == "mask") {
      Mask(as(1))
    } else {
      Mem(as(0), as(1))
    }
  }
}

class Inst

object Mask {
  def apply(str: String): Mask = {
    new Mask(str.toArray)
  }
}

case class Mask(bits: Array[Char]) extends Inst

object Mem {
  def apply(indexStr: String, valueStr: String): Mem = {
    val index = indexStr.drop(4).dropRight(1).toLong
    val value = valueStr.toLong
    Mem(index, value)
  }
}

case class Mem(index: Long, value: Long) extends Inst

val instructions = lines.map(Inst(_)).toArray

object State {
  def initial: State = new State(Mask("X"*36), Map())

  val powers: Array[Long] = {
    (1 to 35).foldLeft(List(1L))((agg, _) => agg match {
      case n :: rest => 2*n :: n :: rest
    }).toArray
  }
}

class State(mask: Mask, mem: Map[Long, Long]) {
  def +(inst: Inst): State = inst match {
    case Mask(ar) => new State(Mask(ar), mem)
    case Mem(i, v) => {
      val vN = (0 until 36).map(i => {
        val p = State.powers(i)
        if ((mask.bits(i) == '1') || ((mask.bits(i) == 'X') && (v/p % 2 == 1)))
          p
        else
          0
      }).sum
      new State(mask, mem + (i -> vN))
    }
  }

  def memory: Map[Long, Long] = mem
}

val last: State = instructions.foldLeft(State.initial)(_ + _)

println(last.memory.values.sum)

object State2 {
  def initial: State2 = new State2(Mask("X"*36), Map())

  val powers: Array[Long] = {
    (1 to 35).foldLeft(List(1L))((agg, _) => agg match {
      case n :: rest => 2*n :: n :: rest
    }).toArray
  }
}

class State2(mask: Mask, mem: Map[Long, Long]) {
  def +(inst: Inst): State2 = inst match {
    case Mask(ar) => new State2(Mask(ar), mem)
    case Mem(i, v) => {
      val memN = (0 to 35).foldLeft(Set(0L))((ls, n) => {
        val p = State2.powers(n)
        if ((mask.bits(n) == '1') || ((mask.bits(n) == '0') && (i/p % 2 == 1)))
          ls.map(_ + p)
        else if (mask.bits(n) == '0')
          ls
        else
          ls.union(ls.map(_ + p))
      }).map((_, v)).toMap
      new State2(mask, mem ++ memN)
    }
  }

  def memory: Map[Long, Long] = mem
}

val last2: State2 = instructions.foldLeft(State2.initial)(_ + _)

println(last2.memory.values.sum)
