#!/usr/bin/env amm

import scala.io.Source
import scala.math

val lines = Source.fromFile("input.txt").getLines.toList

case class Instruction(kind: Char, mag: Int)

object Instruction {
  def apply(str: String): Instruction = {
    Instruction(str(0), str.drop(1).toInt)
  }
}

case class PositionAndHeading(x: Int, y: Int, h: Int) {
  def +(inst: Instruction): PositionAndHeading = {
    inst match {
      case Instruction('N', m) => PositionAndHeading(x, y + m, h)
      case Instruction('S', m) => PositionAndHeading(x, y - m, h)
      case Instruction('E', m) => PositionAndHeading(x + m, y, h)
      case Instruction('W', m) => PositionAndHeading(x - m, y, h)
      case Instruction('L', m) => PositionAndHeading(x, y, (h - m + 360) % 360)
      case Instruction('R', m) => PositionAndHeading(x, y, (h + m) % 360)
      case Instruction('F', m) => this.+(Instruction(degreesToHeading(h), m))
    }
  }

  val degreesToHeading: Map[Int, Char] = Map(
    0 -> 'N',
    90 -> 'E',
    180 -> 'S',
    270 -> 'W'
  )
}

val start = PositionAndHeading(0, 0, 90)

val end = lines.map(Instruction(_)).foldLeft(start)(_ + _)

println(math.abs(end.x) + math.abs(end.y))

case class PosAndWp(x: Int, y: Int, xWp: Int, yWp: Int) {
  def +(inst: Instruction): PosAndWp = {
    inst match {
      case Instruction('N', m) => PosAndWp(x, y, xWp, yWp + m)
      case Instruction('S', m) => PosAndWp(x, y, xWp, yWp - m)
      case Instruction('E', m) => PosAndWp(x, y, xWp + m, yWp)
      case Instruction('W', m) => PosAndWp(x, y, xWp - m, yWp)
      case Instruction('L',  90) => PosAndWp(x, y, -yWp, xWp)
      case Instruction('L', 180) => PosAndWp(x, y, -xWp, -yWp)
      case Instruction('L', 270) => PosAndWp(x, y, yWp, -xWp)
      case Instruction('R', m) => this.+(Instruction('L', 360 - m))
      case Instruction('F', m) => PosAndWp(x + m * xWp, y + m * yWp, xWp, yWp)
    }
  }
}

val start2 = PosAndWp(0 , 0, 10, 1)

val end2 = lines.map(Instruction(_)).foldLeft(start2)(_ + _)

println(math.abs(end2.x) + math.abs(end2.y))
