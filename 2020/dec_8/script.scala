#!/usr/bin/env amm

import scala.io.Source

val inputSrc = Source.fromFile("input.txt")

val lines = inputSrc.getLines

case class Instruction(val inst: String, val value: Int)

object Instruction {
  def apply(str: String): Instruction = {
    val arr = str.split(" ")
    Instruction(arr(0), arr(1).toInt)
  }
}

val instructions = lines.map(Instruction(_)).toArray

class State(val line: Int, val acc: Int, val seen: Set[Int]) {
  def step: State = this.+(instructions(line))

  def +(inst: Instruction): State = {
    inst match {
      case Instruction("nop", _) => new State(line + 1, acc, seen + (line + 1))
      case Instruction("jmp", i) => new State(line + i, acc, seen + (line + i))
      case Instruction("acc", i) => new State(line + 1, acc + i, seen + (line + 1))
    }
  }
}

def stateBeforeEnd(state: State): State = {
  val newState = state.step
  if (state.seen.contains(newState.line)) {
    state
  } else if (newState.line == instructions.length) {
    newState
  } else {
    stateBeforeEnd(newState)
  }
}

val start = new State(0, 0, Set(0))

println(stateBeforeEnd(start).acc)

def correctAccBeforeEnd(state: State): Int = {
  val inst = instructions(state.line)
  inst match {
    case Instruction("acc", i) => {
      correctAccBeforeEnd(state + inst)
    }
    case Instruction(str, i) => {
      val varInst = if (str == "nop")
          Instruction("jmp", i)
        else
          Instruction("nop", i)
      val finalState = stateBeforeEnd(state + varInst)
      if (finalState.line == instructions.length) {
        finalState.acc
      } else {
        correctAccBeforeEnd(state + inst)
      }
    }
  }
}

println(correctAccBeforeEnd(start))
