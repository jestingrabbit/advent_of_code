#!/usr/bin/env amm

import scala.io.Source

val inputSrc = Source.fromFile("input.txt")

val lines = inputSrc.getLines

case class Instruction(inst: String, value: Int)

object Instruction {
  def apply(str: String): Instruction = {
    val arr = str.split(" ")
    Instruction(arr(0), arr(1).toInt)
  }
}

val instructions = lines.map(Instruction(_)).toArray

case class State(line: Int, acc: Int) {
  def step: State = {
    instructions(line) match {
      case Instruction("nop", _) => State(line + 1, acc)
      case Instruction("jmp", i) => State(line + i, acc)
      case Instruction("acc", i) => State(line + 1, acc + i)
    }
  }
}

def acc_before_loop(state: State, seen: Set[Int]): Int = {
  val newState = state.step
  if (seen.contains(newState.line)) {
    state.acc
  } else {
    acc_before_loop(newState, seen + newState.line)
  }
}

println(acc_before_loop(State(0, 0), Set(0)))

def
