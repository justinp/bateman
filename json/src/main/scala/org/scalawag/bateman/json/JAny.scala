package org.scalawag.bateman.json

import cats.Eq
import cats.data.NonEmptyChain
import cats.syntax.validated._
import org.scalawag.bateman.json.JNumber.re

import java.util.regex.Pattern
import scala.util.matching.Regex

sealed trait JType {
  val label: String
}

object JType {
  val all = List(JArray, JObject, JBoolean, JNumber, JString, JNull)
}

sealed trait JAny {
  val jType: JType

  val lineColumnOption: Option[LineColumn]
  val pointerOption: Option[JPointer]

  def forType[A](
      fn: PartialFunction[JType, DecodeResult[A]]
  ): DecodeResult[A] = {
    val validTypes = NonEmptyChain
      .fromSeq(JType.all.filter(fn.isDefinedAt))
      .getOrElse(
        throw new Exception(
          "programmer error: forType partial function must be defined for at least one JType"
        )
      )
    val fail: PartialFunction[JType, DecodeResult[A]] = {
      case _ =>
        JsonTypeMismatch(this, validTypes).invalidNec
    }
    (fn orElse fail)(this.jType)
  }

  def asString: DecodeResult[JString] = JsonTypeMismatch(this, JString).invalidNec
  def asNumber: DecodeResult[JNumber] = JsonTypeMismatch(this, JNumber).invalidNec
  def asNull: DecodeResult[JNull] = JsonTypeMismatch(this, JNull).invalidNec
  def asObject: DecodeResult[JObject] = JsonTypeMismatch(this, JObject).invalidNec
  def asArray: DecodeResult[JArray] = JsonTypeMismatch(this, JArray).invalidNec
  def asBoolean: DecodeResult[JBoolean] = JsonTypeMismatch(this, JBoolean).invalidNec

  def as[A](implicit dec: Decoder[JAny, A]): DecodeResult[A] = dec.decode(this)

  def render: String
}

object JAny {
  sealed trait Parsed extends JAny {

    /** Contains the line/column offset to this item in the JSON text. */
    val lineColumn: LineColumn

    /** Contains the JSON Pointer to the item. */
    val pointer: JPointer

    override val lineColumnOption: Option[LineColumn] = Some(lineColumn)
    override val pointerOption: Option[JPointer] = Some(pointer)
  }

  sealed trait Unparsed extends JAny {
    override val lineColumnOption: Option[LineColumn] = None
    override val pointerOption: Option[JPointer] = None
  }
}

sealed trait JNull extends JAny {
  override val jType: JType = JNull
  override def asNull: DecodeResult[JNull] = this.validNec
  override def render: String = "null"
  override val toString: String = "JNull"
}

case object JNull extends JType with JNull with JAny.Unparsed {
  override val label: String = "null"

  final case class Parsed(
      lineColumn: LineColumn,
      pointer: JPointer
  ) extends JNull
      with JAny.Parsed
}

sealed trait JString extends JAny {
  val s: String
  override val jType: JType = JString
  override def asString: DecodeResult[JString] = this.validNec
  override def render: String = s""""$s"""" // TODO: handle escaping
  override val toString: String = s"""JString("$s")"""
}

case object JString extends JType {
  override val label: String = "a string"

  def apply(s: String): JString = Unparsed(s)

  def unapply(s: JString): Option[String] = Some(s.s)

  final case class Parsed(
      s: String,
      lineColumn: LineColumn,
      pointer: JPointer
  ) extends JString
      with JAny.Parsed

  final case class Unparsed(s: String) extends JString with JAny.Unparsed
}

sealed trait JBoolean extends JAny {
  val b: Boolean
  override val jType: JType = JBoolean
  override def asBoolean: DecodeResult[JBoolean] = this.validNec
  override def render: String = b.toString
  override val toString: String = s"""JBoolean($b)"""
}

case object JBoolean extends JType {
  override val label: String = "a boolean"

  def apply(b: Boolean): JBoolean = Unparsed(b)

  def unapply(b: JBoolean): Option[Boolean] = Some(b.b)

  final case class Parsed(
      b: Boolean,
      lineColumn: LineColumn,
      pointer: JPointer
  ) extends JBoolean
      with JAny.Parsed

  final case class Unparsed(b: Boolean) extends JBoolean with JAny.Unparsed
}

sealed trait JNumber extends JAny {
  val n: String
  override val jType: JType = JNumber
  override def asNumber: DecodeResult[JNumber] = this.validNec
  override def render: String = n
  override val toString: String = s"""JNumber($n)"""

  protected def check = require(re.matcher(n).matches, "input is not a valid JSON number")
}

case object JNumber extends JType {
  override val label: String = "a number"

  private val re: Pattern = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r.pattern

  def unsafe(n: String): JNumber = Unparsed(n)

  def apply(n: String): JNumber = Unparsed(n) // TODO: make this safe but what kind of error should it return?
  def apply(n: Int): JNumber = apply(n.toString)
  def apply(d: Double): JNumber = apply(d.toString)

  def unapply(n: JNumber): Option[String] = Some(n.n)

  final case class Parsed(
      n: String,
      lineColumn: LineColumn,
      pointer: JPointer
  ) extends JNumber
      with JAny.Parsed {
    check
  }

  final case class Unparsed(n: String) extends JNumber with JAny.Unparsed {
    check
  }
}

sealed trait JArray extends JAny {
  val arr: Seq[JAny]
  override val jType: JType = JArray
  override def asArray: DecodeResult[JArray] = this.validNec
  override def render: String = arr.map(_.render).mkString("[", ",", "]")
  override def toString: String = arr.map(_.toString).mkString("JArray(", ",", ")")
}

case object JArray extends JType {
  override val label: String = "an array"

  def apply(arr: JAny*): JArray = Unparsed(arr)

  def unapply(a: JArray): Option[Seq[JAny]] = Some(a.arr)

  final case class Parsed(
      arr: Seq[JAny.Parsed],
      lineColumn: LineColumn,
      pointer: JPointer
  ) extends JArray
      with JAny.Parsed

  final case class Unparsed(arr: Seq[JAny]) extends JArray with JAny.Unparsed
}

sealed trait JField {
  val name: JString
  val value: JAny
  def render: String = s"${name.render}:${value.render}"
  override def toString: String = s"$name -> $value"
}

object JField {
  def apply(name: JString, value: JAny): JField = Unparsed(name, value)

  def unapply(f: JField): Option[(JString, JAny)] = Some(f.name, f.value)

  case class Parsed(name: JString.Parsed, value: JAny.Parsed) extends JField
  case class Unparsed(name: JString, value: JAny) extends JField
}

sealed trait JObject extends JAny {
  val fields: Seq[JField]
  override val jType: JType = JObject
  override def asObject: DecodeResult[JObject] = this.validNec
  override def render: String = fields.map(_.render).mkString("{", ",", "}")
  override def toString: String = fields.map(_.toString).mkString("JObject(", ",", ")")

  def getField(fieldName: String): Option[JField] =
    fields.find {
      case JField(JString(k), v) if k == fieldName => true
      case _                                       => false
    }

  def optionalField(fieldName: String): Option[JAny] =
    getField(fieldName).map(_.value)

  def field(fieldName: String): DecodeResult[JAny] =
    optionalField(fieldName).map(_.validNec).getOrElse {
      UnspecifiedField(this, fieldName).invalidNec
    }

  def fieldOr(fieldName: String, default: JAny): JAny =
    optionalField(fieldName).getOrElse(default)

  def validateFields(allowedNames: Iterable[String]): DecodeResult[Unit] = {
    val allowedNamesSet = allowedNames.toSet
    val unexpectedNames =
      fields.map(_.name).filterNot(s => allowedNamesSet.contains(s.s))
    NonEmptyChain
      .fromSeq(unexpectedNames)
      .map(ss => ss.map(s => UnexpectedField(s)).invalid)
      .getOrElse(().validNec)
  }

  def -(fieldName: String): JObject
  def +(field: (String, JAny)): JObject.Unparsed =
    JObject.Unparsed(fields = fields :+ JField(JString(field._1), field._2))
}

case object JObject extends JType {
  override val label: String = "an object"

  def apply(fields: Iterable[JField]): JObject = Unparsed(fields.toSeq)
  def apply(fields: (String, JAny)*): JObject = apply(fields.map { case (k, v) => JField(JString(k), v) })

  def unapply(o: JObject): Option[Seq[JField]] = Some(o.fields)

  final case class Parsed(
      fields: Seq[JField.Parsed],
      lineColumn: LineColumn,
      pointer: JPointer
  ) extends JObject
      with JAny.Parsed {
    def -(fieldName: String): JObject.Parsed = copy(fields = fields.filterNot(_.name.s == fieldName))
  }

  final case class Unparsed(fields: Seq[JField]) extends JObject with JAny.Unparsed {
    def -(fieldName: String): JObject.Unparsed = copy(fields = fields.filterNot(_.name.s == fieldName))
  }

  object Equality {
    implicit val sameFieldsAnyOrder: Eq[JObject] = { (l, r) =>
      val lm = l.fields.map { case JField(k, v) => k.s -> v }.toMap
      val rm = r.fields.map { case JField(k, v) => k.s -> v }.toMap
      lm == rm
    }
  }
}
