package org.scalawag.bateman.json

/** Represents a JSON Pointer (https://tools.ietf.org/html/rfc6901). */

final case class JPointer(tokens: List[String]) {
  def +(s: String): JPointer = JPointer(s :: this.tokens)
  def parent: JPointer = JPointer(this.tokens.tail)
  override val toString: String = "/" + tokens.reverse.mkString("/") // TODO: handle escapes
}

object JPointer {
  val Root = JPointer(Nil)
}
