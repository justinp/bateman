package org.scalawag.bateman.json.generic.naming

import scala.annotation.tailrec

trait Case {
  def toWords(s: String): List[String]
  def fromWords(words: List[String]): String

  protected def capitalize(word: String): String = word.take(1).map(_.toUpper) ++ word.drop(1)
}

trait CaseBasedCase extends Case {
  override def toWords(s: String): List[String] = {
    @tailrec
    def rec(remains: String, acc: List[String]): List[String] =
      if (remains.isEmpty)
        acc
      else {
        val next = remains.take(1).toLowerCase ++ remains.drop(1).takeWhile(_.isLower)
        rec(remains.drop(next.length), next :: acc)
      }

    rec(s, Nil).reverse
  }
}

object CamelCase extends CaseBasedCase {
  override def fromWords(words: List[String]): String = words.take(1) + words.drop(1).map(capitalize).mkString
}

object PascalCase extends CaseBasedCase {
  override def fromWords(words: List[String]): String = words.map(capitalize).mkString
}

abstract class DelimiterBasedCase(delimiter: Char) extends Case {
  private val delimiterString = delimiter.toString
  override def toWords(s: String): List[String] = s.split(delimiter).map(_.toLowerCase).toList
  override def fromWords(words: List[String]): String = words.mkString(delimiterString)
}

object KebabCase extends DelimiterBasedCase('-')
object SnakeCase extends DelimiterBasedCase('_')

case class CaseTransformation(from: Case, to: Case) extends Function[String, String] {
  override def apply(in: String): String = to.fromWords(from.toWords(in))
}
