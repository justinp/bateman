package org.scalawag.bateman.json.parser

import org.scalawag.bateman.json.LineColumn
import fastparse.Parsed

final case class SyntaxError(lineColumn: LineColumn, failure: Parsed.Failure) {
  val description: String = s"syntax error: ${failure.msg}"
}
