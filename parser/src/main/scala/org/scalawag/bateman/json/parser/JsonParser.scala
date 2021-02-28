package org.scalawag.bateman.json.parser

import cats.syntax.validated._
import org.scalawag.bateman.json._
import fastparse.NoWhitespace._
import fastparse._

import scala.io.Source

object JsonParser {
  def parse(text: String, source: Option[String] = None): ParseResult[JAny] =
    new JsonParser(text, source).parse.map(addPointers)

  // Do this all at once after parsing so that we don't have to maintain the state down through the parser.
  private def addPointers(j: JAny.Parsed): JAny = {
    def go(pointer: JPointer, jany: JAny.Parsed): JAny.Parsed =
      jany match {
        case x: JNull.Parsed    => x.copy(pointer = pointer)
        case x: JBoolean.Parsed => x.copy(pointer = pointer)
        case x: JString.Parsed  => x.copy(pointer = pointer)
        case x: JNumber.Parsed  => x.copy(pointer = pointer)
        case x: JObject.Parsed =>
          x.copy(
            fields = x.fields.map { f =>
              val p = pointer + f.name.s
              f.copy(
                name = f.name.copy(pointer = p),
                value = go(p, f.value)
              )
            },
            pointer = pointer
          )
        case x: JArray.Parsed =>
          x.copy(
            arr = x.arr.zipWithIndex.map {
              case (v, n) =>
                go(pointer + n.toString, v)
            },
            pointer = pointer
          )
      }

    go(JPointer.Root, j)
  }
}

class JsonParser private (input: String, source: Option[String] = None) {

  private def ws[_: P] = P(CharIn("\t\r\n ").rep(1))

  private def hexDigit[_: P] = P(CharIn("0-9", "a-f", "A-F"))

  private def twoCharacterEscape[_: P]: P[Char] =
    P(escapedCharacter.map {
      case c @ ("\"" | "/" | "\\") => c.head
      case "b"                     => '\b'
      case "f"                     => '\f'
      case "r"                     => '\r'
      case "n"                     => '\n'
      case "t"                     => '\t'
    })

  private def sixCharacterEscape[_: P]: P[Char] =
    P("u" ~ hexDigit.rep(4, "", 4).!).map { s =>
      java.lang.Integer.parseInt(s, 16).toChar
    }

  private def escapedCharacter[_: P]: P[String] =
    P((CharIn("\\\"/\\\\bfnrt").!))

  private def escapeSequence[_: P]: P[Char] =
    P("\\" ~/ (twoCharacterEscape | sixCharacterEscape))

  // strings: TODO: exclude control characters (U+0000 to U+001F) here
  private def quote[_: P] = P("\"")
  private def notQuote[_: P] = P(CharPred(_ != '"').!.map(_.head))
  private def quotedStringChars[_: P]: P[String] =
    P((escapeSequence | notQuote).rep).map(_.mkString)
  private def string[_: P] =
    P(Index ~ quote ~ quotedStringChars ~ quote).map {
      case (index, s) =>
        JString.Parsed(s, lineColumnForIndex(index), null)
    }

  private def comma[_: P] = P(ws.? ~ "," ~/ ws.?)

  private def array[_: P]: P[JArray.Parsed] =
    P(Index ~ "[" ~/ jsonExpr.rep(sep = comma) ~ "]").map {
      case (index, items) =>
        // Prepend the index onto each of the pointers contained in here.
        JArray.Parsed(items, lineColumnForIndex(index), null)
    }

  private def field[_: P]: P[JField.Parsed] =
    P {
      (string ~ ws.? ~ ":" ~ ws.? ~ jsonExpr).map(JField.Parsed.tupled)
    }

  private def `object`[_: P]: P[JObject.Parsed] =
    P {
      (Index ~ "{" ~/ ws.? ~ field.rep(sep = comma) ~ ws.? ~/ "}").map {
        case (index, fields) =>
          JObject.Parsed(fields, lineColumnForIndex(index), null)
      }
    }

  private def `"null"`[_: P]: P[JNull.Parsed] =
    P {
      (Index ~ "null").map { index =>
        JNull.Parsed(lineColumnForIndex(index), null)
      }
    }

  private def boolean[_: P]: P[JBoolean.Parsed] =
    P {
      (Index ~ ("true" | "false").!).map {
        case (index, b) =>
          JBoolean.Parsed(b.toBoolean, lineColumnForIndex(index), null)
      }
    }

  private def digit[_: P] = P(CharIn("0-9"))
  private def digits[_: P] = P(digit.rep(1))
  private def sign[_: P] = P("-")
  private def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)
  private def fractional[_: P] = P("." ~ digits)
  private def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

  private def number[_: P]: P[JNumber.Parsed] =
    P {
      (Index ~ (sign.? ~ integral ~/ fractional.? ~ exponent.?).!).map {
        case (index, s) =>
          JNumber.Parsed(s, lineColumnForIndex(index), null)
      }
    }

  private def jsonExpr[_: P]: P[JAny.Parsed] =
    P {
      ws.? ~ (`object` | array | `"null"` | boolean | number | string) ~ ws.?
    }

  private def root[_: P] = P(jsonExpr ~ End)

  def parse: ParseResult[JAny.Parsed] =
    fastparse.parse(input, root(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => value.valid
      case f: Parsed.Failure =>
        SyntaxError(lineColumnForIndex(f.index), f).invalidNec
    }

  // This is just a list of the index offset for the beginning of each line to speed up the LineColumn derivation.
  private val lineForIndex: List[Int] =
    Source
      .fromString(input)
      .getLines
      .map(_.length)
      .scanLeft(0)(_ + _ + 1)
      .toList

  private def lineColumnForIndex(index: Int): LineColumn = {
    val (offset, line) = lineForIndex.zipWithIndex.takeWhile(_._1 <= index).last
    LineColumn(line + 1, index - offset + 1)
  }
}
