package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{JString, JPointer, LineColumn, SchemaError}
import org.scalawag.bateman.json.{JPointer, LineColumn, SchemaError}

case class InvalidDiscriminator(discriminator: JString, valids: Iterable[String]) extends SchemaError {
  override def lineColumn: Option[LineColumn] = discriminator.lineColumnOption
  override def pointer: Option[JPointer] = discriminator.pointerOption
  override def description: String = s"discriminator must be one of the following: ${valids.mkString(", ")}"
}

case class DiscriminatorFieldCollision(name: String, subclass: String) extends Exception with SchemaError {
  override def lineColumn: Option[LineColumn] = None
  override def pointer: Option[JPointer] = None
  override def description: String =
    s"discriminator field '$name' collides with a field on concrete subclass '$subclass'"
}
