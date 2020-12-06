#!/usr/bin/env amm

import scala.io.Source

val raw_data = Source.fromFile("input.txt").mkString

object Passport {
  def apply(str: String): Map[String, String] = {
    "\\s".r.split(str)
      .map(p => p.split(":") match { case Array(a, b) => (a, b) })
      .toMap
  }

  val essentialKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val optionalKey = "cid"

  def isValid(p: Map[String, String]): Boolean = {
    val ks = p.keys.toSet
    ks.sameElements(essentialKeys) || ks.sameElements(essentialKeys + optionalKey)
  }

  def isYear(str: String):Boolean = {
    """\d{4}""".r.matches(str)
  }

  def isMetric(str: String):Boolean = {
    """\d+cm""".r.matches(str)
  }

  def isImperial(str: String):Boolean = {
    """\d+in""".r.matches(str)
  }

  def between(str: String, low: Int, high: Int): Boolean = {
    val i = str.toInt
    low <= i && high >= i
  }

  def isYearBetween(low: Int, high: Int): Function[String, Boolean] = {
    (s => isYear(s) && between(s, low, high))
  }

  def isValidHeight(str: String): Boolean = {
    isMetric(str) && between(str.dropRight(2), 150, 193) ||
    isImperial(str) && between(str.dropRight(2), 59, 76)
  }

  def isHexColor(str: String): Boolean = {
    """#[\da-f]{6}""".r.matches(str)
  }

  val abbrColors: Set[String] = "amb blu brn gry grn hzl oth".split(" ").toSet

  def isAbbrColor(str: String): Boolean = {
    abbrColors.contains(str)
  }

  def isId(str: String): Boolean = {
    """\d{9}""".r.matches(str)
  }

  val rules: Map[String, Function[String, Boolean]] = Map(
    "byr" -> isYearBetween(1920, 2002),
    "iyr" -> isYearBetween(2010, 2020),
    "eyr" -> isYearBetween(2020, 2030),
    "hgt" -> isValidHeight,
    "hcl" -> isHexColor,
    "ecl" -> isAbbrColor,
    "pid" -> isId
  )

  val defaultF: Function[String, Boolean] = (s => false)

  def isValid2(p: Map[String, String]): Boolean = {
    essentialKeys.forall(k => rules.getOrElse(k, defaultF)(p.getOrElse(k, "")))
  }
}

val passports = raw_data.split("\n\n").map(Passport(_))

println(passports.count(Passport.isValid).toString)

println(passports.count(Passport.isValid2).toString)
