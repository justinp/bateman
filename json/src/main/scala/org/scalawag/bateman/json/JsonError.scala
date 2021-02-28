package org.scalawag.bateman.json

import cats.data.NonEmptyChain

trait SchemaError {
  def lineColumn: Option[LineColumn]
  def pointer: Option[JPointer]
  def description: String
  def fullDescription: String = s"${lineColumn}: $description ($pointer)"
}

final case class GenericSchemaError(
    lineColumn: Option[LineColumn],
    pointer: Option[JPointer],
    description: String
) extends SchemaError

final case class FailedAssumption(
    lineColumn: Option[LineColumn],
    pointer: Option[JPointer],
    s: String
) extends SchemaError {
  override def description: String = s"programming error: ${s}"
}

final case class InvalidValue(value: JAny, description: String) extends SchemaError {
  override def pointer: Option[JPointer] = value.pointerOption
  override def lineColumn: Option[LineColumn] = value.lineColumnOption
}

final case class UnspecifiedField(obj: JObject, fields: NonEmptyChain[String])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = obj.pointerOption
  override def lineColumn: Option[LineColumn] = obj.lineColumnOption
  override val description =
    s"JSON object is missing required field ${formatOrList(fields.iterator.map(quote))}."
}

object UnspecifiedField {
  def apply(obj: JObject, field: String): UnspecifiedField =
    UnspecifiedField(obj, NonEmptyChain.one(field))
}

final case class UnexpectedField(key: JString) extends SchemaError {
  override def pointer: Option[JPointer] = key.pointerOption
  override def lineColumn: Option[LineColumn] = key.lineColumnOption
  override val description =
    s"JSON key is not allowed here."
}

final case class JsonTypeMismatch(actual: JAny, expected: NonEmptyChain[JType])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = actual.pointerOption
  override def lineColumn: Option[LineColumn] = actual.lineColumnOption

  override val description: String =
    s"A JSON value of type ${formatOrList(expected.iterator.map(_.toString))} is expected here, " +
      s"but one of type '${actual.jType}' was found instead."
}

object JsonTypeMismatch {
  def apply(actual: JAny, expected: JType): JsonTypeMismatch = apply(actual, NonEmptyChain(expected))
}

trait ErrorFormatters {
  protected def quote(s: String): String = s""""$s""""

  protected def formatOrList(items: Iterator[String]): String =
    formatList("or", items)

  protected def formatAndList(items: Iterator[String]): String =
    formatList("and", items)

  protected def formatList(
      conjunction: String,
      items: Iterator[String]
  ): String =
    items.toList match {
      case head :: Nil => head
      case itemList =>
        itemList
          .dropRight(1)
          .mkString("", ", ", s" $conjunction ") + itemList.last
    }
}
