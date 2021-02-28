package org.scalawag.bateman.jsonapi

import cats.syntax.validated._
import cats.data.NonEmptyChain
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.instances.option._
import cats.instances.list._
import org.scalawag.bateman.json.{DecodeResult, Decoder, JAny, JArray, JType, JNull, JObject, JString, UnexpectedField}

final case class Meta(mappings: Map[JString, JAny]) {
  private val bareMap: Map[String, JAny] = mappings.map { case (k, v) => k.s -> v }
  def get(key: String): Option[JAny] = bareMap.get(key)
}

object Meta {
  implicit val decoder: Decoder[JAny, Meta] = Decoder[JAny, Map[JString, JAny]].map(Meta.apply)
}

final case class Links(mappings: Map[JString, Link]) {
  private val bareMap: Map[String, Link] = mappings.map { case (k, v) => k.s -> v }
  def get(key: String): Option[Link] = bareMap.get(key)
}

object Links {
  implicit val decoder: Decoder[JAny, Links] = Decoder[JAny, Map[JString, Link]].map(Links.apply)
}

sealed trait Link

object Link {
  implicit val decoder: Decoder[JAny, Link] = { in =>
    in.forType {
      case JString => Decoder[JAny, BareLink].decode(in)
      case JObject => Decoder[JAny, RichLink].decode(in)
    }
  }
}

final case class BareLink(href: String) extends Link

object BareLink {
  implicit val decoder: Decoder[JAny, BareLink] = Decoder[JAny, String].map(BareLink.apply)
}

final case class RichLink(href: Option[String] = None, meta: Option[Meta] = None) extends Link

object RichLink {
  implicit val decoder: Decoder[JAny, RichLink] = _.asObject.andThen { obj =>
    (
      obj.optionalField("href").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("meta").map(Decoder[JAny, Meta].decode).sequence
    ).mapN(RichLink.apply)
  }
}

final case class ErrorSource(pointer: Option[String] = None, parameter: Option[String] = None)

object ErrorSource {
  implicit val decoder: Decoder[JAny, ErrorSource] = _.asObject.andThen { obj =>
    (
      obj.optionalField("pointer").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("parameter").map(Decoder[JAny, String].decode).sequence
    ).mapN(ErrorSource.apply)
  }
}

final case class Error(
    id: Option[String] = None,
    links: Option[Map[String, Link]] = None,
    status: Option[String] = None,
    code: Option[String] = None,
    title: Option[String] = None,
    detail: Option[String] = None,
    source: Option[ErrorSource] = None,
    meta: Option[Map[String, JAny]] = None
)

object Error {
  implicit val decoder: Decoder[JAny, Error] = _.asObject.andThen { obj =>
    (
      obj.optionalField("id").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("links").map(Decoder[JAny, Map[String, Link]].decode).sequence,
      obj.optionalField("status").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("code").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("title").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("detail").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("source").map(Decoder[JAny, ErrorSource].decode).sequence,
      obj.optionalField("meta").map(Decoder[JAny, Map[String, JAny]].decode).sequence
    ).mapN(Error.apply)
  }
}

final case class Jsonapi(version: Option[String] = None, meta: Option[Meta] = None)

object Jsonapi {
  implicit val decoder: Decoder[JAny, Jsonapi] = _.asObject.andThen { obj =>
    (
      obj.optionalField("version").map(Decoder[JAny, String].decode).sequence,
      obj.optionalField("meta").map(Decoder[JAny, Meta].decode).sequence
    ).mapN(Jsonapi.apply)
  }
}

abstract class DataCardinalities[A](implicit decoder: Decoder[JAny, A]) {
  protected def requiredData: DecodeResult[JAny]
  protected def cardinalityMismatch(expected: NonEmptyChain[JType]): DecodeResult[Nothing]

  def datum: DecodeResult[A] =
    requiredData andThen {
      case o: JObject => o.as[A]
      case _          => cardinalityMismatch(NonEmptyChain(JObject))
    }

  def nullableDatum: DecodeResult[Option[A]] =
    requiredData andThen {
      case in: JObject => in.as[A].map(Some.apply)
      case JNull       => None.validNec
      case _           => cardinalityMismatch(NonEmptyChain(JObject, JNull))
    }

  def arrayData: DecodeResult[List[A]] =
    requiredData andThen {
      case a: JArray => a.arr.toList.map(_.as[A]).sequence[DecodeResult, A]
      case _         => cardinalityMismatch(NonEmptyChain(JArray))
    }
}

final case class Relationship(
    src: JObject,
    data: Option[JAny] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None
) extends DataCardinalities[ResourceIdentifier] {

  override protected def cardinalityMismatch(expected: NonEmptyChain[JType]): DecodeResult[Nothing] =
    requiredData andThen { d =>
      ResourceLinkageCardinalityMismatch(d, expected).invalidNec
    }

  override protected def requiredData: DecodeResult[JAny] =
    data.map(_.validNec).getOrElse(MissingResourceLinkage(this).invalidNec)
}

object Relationship {
  implicit val decoder: Decoder[JAny, Relationship] = _.asObject.andThen { obj =>
    (
      obj.validNec: DecodeResult[JObject],
      obj.optionalField("data").map(Decoder[JAny, JAny].decode).sequence,
      obj.optionalField("meta").map(Decoder[JAny, Meta].decode).sequence,
      obj.optionalField("links").map(Decoder[JAny, Links].decode).sequence
    ).mapN(Relationship.apply)
  }
}

sealed trait ResourceIdentifierLike {
  val src: JObject
  val `type`: JString
  val meta: Option[Meta]

  def validateType(expectedType: String): DecodeResult[Unit] =
    if (`type`.s != expectedType)
      JsonApiTypeMismatch(this, expectedType).invalidNec
    else
      ().validNec
}

final case class ResourceObject(
    src: JObject,
    `type`: JString,
    id: Option[JString] = None,
    attributes: Option[Map[JString, JAny]] = None,
    relationships: Option[Map[JString, Relationship]] = None,
    meta: Option[Meta] = None,
    links: Option[Links] = None
) extends ResourceIdentifierLike {

  def optionalId: DecodeResult[Option[JString]] =
    id.map(_.validNec).sequence[DecodeResult, JString]
  def optionalIdAs[A](implicit dec: Decoder[JString, A]): DecodeResult[Option[A]] =
    id.map(dec.decode).sequence
  def requiredId: DecodeResult[JString] =
    id.map(_.validNec).getOrElse(MissingId(this).invalidNec)
  def requiredIdAs[A](implicit dec: Decoder[JString, A]): DecodeResult[A] =
    requiredId.andThen(dec.decode)

  def optionalAttribute(name: String): Option[JAny] =
    attributes.flatMap(_.collectFirst { case (k, v) if k.s == name => v })
  def optionalAttributeAs[A](name: String)(implicit dec: Decoder[JAny, A]): DecodeResult[Option[A]] =
    optionalAttribute(name).map(dec.decode).sequence
  def requiredAttribute(name: String): DecodeResult[JAny] =
    optionalAttribute(name).map(_.validNec).getOrElse(MissingAttribute(this, name).invalidNec)
  def requiredAttributeAs[A](name: String)(implicit dec: Decoder[JAny, A]): DecodeResult[A] =
    requiredAttribute(name).andThen(dec.decode)

  def optionalRelationship(name: String): Option[Relationship] =
    relationships.flatMap(_.collectFirst { case (k, v) if k.s == name => v })
  def optionalRelationshipAs[A](name: String)(implicit dec: Decoder[Relationship, A]): DecodeResult[Option[A]] =
    optionalRelationship(name).map(dec.decode).sequence
  def requiredRelationship(name: String): DecodeResult[Relationship] =
    optionalRelationship(name).map(_.validNec).getOrElse(MissingRelationship(this, name).invalidNec)
  def requiredRelationshipAs[A](name: String)(implicit dec: Decoder[Relationship, A]): DecodeResult[A] =
    requiredRelationship(name).andThen(dec.decode)

  def as[A](implicit dec: Decoder[ResourceObject, A]): DecodeResult[A] = dec.decode(this)
}

object ResourceObject {
  implicit val decoder: Decoder[JAny, ResourceObject] = _.asObject.andThen { obj =>
    (
      obj.validNec: DecodeResult[JObject],
      obj.field("type").andThen(Decoder[JAny, JString].decode): DecodeResult[JString],
      obj.optionalField("id").map(Decoder[JAny, JString].decode).sequence,
      obj.optionalField("attributes").map(Decoder[JAny, Map[JString, JAny]].decode).sequence,
      obj.optionalField("relationships").map(Decoder[JAny, Map[JString, Relationship]].decode).sequence,
      obj.optionalField("meta").map(Decoder[JAny, Meta].decode).sequence,
      obj.optionalField("links").map(Decoder[JAny, Links].decode).sequence
    ).mapN(ResourceObject.apply)
  }
}

final case class ResourceIdentifier(src: JObject, `type`: JString, id: JString, meta: Option[Meta] = None)
    extends ResourceIdentifierLike

object ResourceIdentifier {
  implicit val decoder: Decoder[JAny, ResourceIdentifier] = _.asObject.andThen { obj =>
    (
      obj.validNec: DecodeResult[JObject],
      obj.field("type").andThen(Decoder[JAny, JString].decode): DecodeResult[JString],
      obj.field("id").andThen(Decoder[JAny, JString].decode): DecodeResult[JString],
      obj.optionalField("meta").map(Decoder[JAny, Meta].decode).sequence
    ).mapN(ResourceIdentifier.apply)
  }
}

case class Included(asList: List[ResourceObject], asMap: Map[String, Map[String, ResourceObject]])

object Included {
  def apply(included: List[ResourceObject]): DecodeResult[Included] = {
    // Trust me. :)
    included
      .groupBy(_.`type`.s)
      .map {
        case (resourceType, resourceObjects) =>
          resourceObjects
            .map(inc => inc.requiredId.map(_.s -> inc))
            .sequence[DecodeResult, (String, ResourceObject)]
            .andThen { idObjectPairs =>
              val duplicateSets = idObjectPairs.groupBy(_._1).mapValues(_.map(_._2)).filter(_._2.size > 1).values
              val duplicateErrors =
                duplicateSets.flatMap { dups =>
                  dups.map { dup =>
                    DuplicateResourceObjectDefinition(dup, NonEmptyChain.fromSeq(dups.filter(_ == dup)).get)
                  }
                }
              NonEmptyChain.fromSeq(duplicateErrors.toSeq) match {
                case Some(ee) => ee.invalid
                case None     => (resourceType -> idObjectPairs.toMap).validNec
              }
            }
      }
      .toList
      .sequence[DecodeResult, (String, Map[String, ResourceObject])]
      .map(_.toMap)
      .map(Included(included, _))
  }
}

// None for all these None means that the key didn't appear in the document. If they're set to "null" or empty, they
// will have a Some.

final case class Document(
    src: JObject,
    data: Option[JAny] = None,
    errors: Option[List[Error]] = None,
    meta: Option[Meta] = None,
    jsonapi: Option[Jsonapi] = None,
    links: Option[Links] = None,
    included: Option[Included] = None
) extends DataCardinalities[ResourceObject] {
  override protected def requiredData: DecodeResult[JAny] =
    data.map(_.validNec).getOrElse(MissingPrimaryData(this).invalidNec)

  override protected def cardinalityMismatch(expected: NonEmptyChain[JType]): DecodeResult[Nothing] =
    requiredData.andThen { d => PrimaryDataCardinalityMismatch(d, expected).invalidNec }

  def optionalIncluded(identifier: ResourceIdentifier): Option[ResourceObject] =
    for {
      byType <- included.map(_.asMap)
      byId <- byType.get(identifier.`type`.s)
      resourceObject <- byId.get(identifier.id.s)
    } yield resourceObject

  def requiredIncluded(identifier: ResourceIdentifier): DecodeResult[ResourceObject] =
    optionalIncluded(identifier).map(_.validNec).getOrElse(MissingIncludedResourceObject(identifier).invalidNec)
}

object Document {
  implicit val fromObject: Decoder[JObject, Document] = { obj =>
    (
      obj.validNec: DecodeResult[JObject],
      obj.optionalField("data").map(Decoder[JAny, JAny].decode).sequence,
      obj.optionalField("errors").map(Decoder[JAny, List[Error]].decode).sequence,
      obj.optionalField("meta").map(Decoder[JAny, Meta].decode).sequence,
      obj.optionalField("jsonapi").map(Decoder[JAny, Jsonapi].decode).sequence,
      obj.optionalField("links").map(Decoder[JAny, Links].decode).sequence,
      obj
        .optionalField("included")
        .map(Decoder[JAny, List[ResourceObject]].decode(_).andThen(Included(_)))
        .sequence[DecodeResult, Included]
    ).mapN(Document.apply)
  }

  implicit val fromAny: Decoder[JAny, Document] = _.asObject.andThen(fromObject.decode)
}

trait DocumentDecodingUtils {
  def detectExtraneousFields(obj: JObject, allowedFieldNames: String => Boolean): DecodeResult[Unit] = {
    val unexpectedFieldNames = obj.fields.map(_.name).filterNot(n => allowedFieldNames(n.s))
    NonEmptyChain
      .fromSeq(unexpectedFieldNames)
      .map(names => names.map(name => UnexpectedField(name)).invalid)
      .getOrElse(().validNec)
  }
}
