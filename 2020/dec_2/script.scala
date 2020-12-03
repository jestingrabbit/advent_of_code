#!/usr/bin/env amm

import scala.io.Source

object Policy {
  def apply(str: String): Policy = {
    str.split(" ") match {
      case Array(limits, char_str) => {
        val char: Char = char_str.toList.head
        limits.split("-") match {
          case Array(min_str, max_str) => Policy(char, min_str.toInt, max_str.toInt)
          case _ => Policy(' ', 0, 0)
        }
      }
      case _ => Policy(' ', 0, 0)
    }
  }
}

case class Policy(char: Char, min: Int, max: Int) {
  def valid(password: String): Boolean = {
    val c: Int = password.count(_ == char)
    min <= c && c <= max
  }

  def valid2(password: String): Boolean ={
    val chars: List[Char] = password.toList
    (chars(min-1) == char) != (chars(max-1) == char)
  }
}

val lines = Source
  .fromFile("input.txt")
  .getLines

def count_valid_passwords(lines: Iterator[String]): Int = {
  lines.count(line => line.split(": ") match {
    case Array(policy_str, password) => Policy(policy_str).valid(password)
    case _ => false
  })
}

printf("%d\n", count_valid_passwords(lines))

val lines2 = Source
  .fromFile("input.txt")
  .getLines

def count_valid_passwords2(lines: Iterator[String]): Int = {
  lines.count(line => line.split(": ") match {
    case Array(policy_str, password) => Policy(policy_str).valid2(password)
    case _ => false
  })
}

printf("%d\n", count_valid_passwords2(lines2))
