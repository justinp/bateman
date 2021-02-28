package org.scalawag.bateman.jsonapi

import cats.data.NonEmptyChain
import org.scalawag.bateman.json.{ErrorFormatters, JAny, JPointer, JType, LineColumn, SchemaError}

final case class JsonApiTypeMismatch(resource: ResourceIdentifierLike, expected: NonEmptyChain[String])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = resource.src.pointerOption
  override def lineColumn: Option[LineColumn] = resource.src.lineColumnOption
  override val description: String =
    s"A JSON:API resource object of type ${formatOrList(expected.iterator.map(x => s"'$x'"))} is expected here, " +
      s"but one of type '${resource.`type`.s}' was found instead."
}

object JsonApiTypeMismatch {
  def apply(resource: ResourceIdentifierLike, expected: String): JsonApiTypeMismatch =
    JsonApiTypeMismatch(resource, NonEmptyChain.one(expected))
}

final case class MissingId(resource: ResourceObject) extends SchemaError with ErrorFormatters {
  override def pointer: Option[JPointer] = resource.src.pointerOption
  override def lineColumn: Option[LineColumn] = resource.src.lineColumnOption
  override val description: String =
    s"A JSON:API resource object is missing its required id."
}

final case class MissingAttribute(resource: ResourceObject, expected: NonEmptyChain[String])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = resource.src.pointerOption
  override def lineColumn: Option[LineColumn] = resource.src.lineColumnOption
  override val description: String =
    s"A JSON:API resource object is missing a required attribute ${formatOrList(expected.iterator.map(x => s"'$x'"))}."
}

object MissingAttribute {
  def apply(resource: ResourceObject, expected: String): MissingAttribute =
    MissingAttribute(resource, NonEmptyChain.one(expected))
}

final case class MissingRelationship(resource: ResourceObject, expected: NonEmptyChain[String])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = resource.src.pointerOption
  override def lineColumn: Option[LineColumn] = resource.src.lineColumnOption
  override val description: String =
    s"A JSON:API resource object is missing a required relationship ${formatOrList(expected.iterator.map(x => s"'$x'"))}."
}

object MissingRelationship {
  def apply(resource: ResourceObject, expected: String): MissingRelationship =
    MissingRelationship(resource, NonEmptyChain.one(expected))
}

final case class MissingPrimaryData(document: Document) extends SchemaError {
  override def pointer: Option[JPointer] = document.src.pointerOption
  override def lineColumn: Option[LineColumn] = document.src.lineColumnOption
  override val description: String =
    "The JSON:API document is missing its required primary 'data'."
}

final case class PrimaryDataCardinalityMismatch(data: JAny, expected: NonEmptyChain[JType])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = data.pointerOption
  override def lineColumn: Option[LineColumn] = data.lineColumnOption
  override val description: String =
    s"The primary data for this JSON:API document contains ${data.jType.label} when it should contain ${formatOrList(expected.iterator.map(_.label))}."
}

object PrimaryDataCardinalityMismatch {
  def apply(data: JAny, expected: JType): PrimaryDataCardinalityMismatch =
    PrimaryDataCardinalityMismatch(data, NonEmptyChain.one(expected))
}

final case class MissingResourceLinkage(relationship: Relationship) extends SchemaError {
  override def pointer: Option[JPointer] = relationship.src.pointerOption
  override def lineColumn: Option[LineColumn] = relationship.src.lineColumnOption
  override val description: String =
    "A JSON:API relationship is missing a required 'data' (resource linkage)."
}

final case class ResourceLinkageCardinalityMismatch(data: JAny, expected: NonEmptyChain[JType])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = data.pointerOption
  override def lineColumn: Option[LineColumn] = data.lineColumnOption
  override val description: String =
    s"The resource linkage for this JSON:API relationship contains ${data.jType.label} when it should contain ${formatOrList(expected.iterator.map(_.label))}."
}

object ResourceLinkageCardinalityMismatch {
  def apply(data: JAny, expected: JType): ResourceLinkageCardinalityMismatch =
    ResourceLinkageCardinalityMismatch(data, NonEmptyChain.one(expected))
}

final case class DuplicateResourceObjectDefinition(obj: ResourceObject, others: NonEmptyChain[ResourceObject])
    extends SchemaError
    with ErrorFormatters {
  override def pointer: Option[JPointer] = obj.src.pointerOption
  override def lineColumn: Option[LineColumn] = obj.src.lineColumnOption
  override val description: String =
    s"This resource object is defined elsewhere in the document: ${others.iterator.map(_.src.pointerOption).mkString(" ")}."
}

final case class MissingIncludedResourceObject(identifier: ResourceIdentifier) extends SchemaError {
  override def pointer: Option[JPointer] = identifier.src.pointerOption
  override def lineColumn: Option[LineColumn] = identifier.src.lineColumnOption
  override val description: String =
    s"Missing included resource object definition for resource identifier (type=${identifier.`type`} id=${identifier.id}."
}
