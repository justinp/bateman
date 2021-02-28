package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.{DecodeResult, Decoder}
import shapeless.{Lazy, tag}
import shapeless.tag.@@

// These are our marker tags to indicate when a case class field should be treated as the ID vs. an attribute or a
// relationship.
//
// They all have autowrapping implicits because, as the traits themselves have no requirements, we may as well wrap
// anything that needs it. This is what keeps us from having to explicitly set the tag every time we assign a value
// to a tagged case class field.
//
// They also each have an implicit decoder that makes it so that any Decoders for the target types _without_ the tag
// will work for any types _with_ the tag by simply using the decoder and then reapplying the tag.

sealed trait Marker

sealed trait Id extends Marker

object Id {
  implicit def autoWrap[A](a: A): A @@ Id = tag[Id](a)

  trait MarkerDecoder[A, B] extends Decoder[A, B]

  implicit def idTagDecoder[A, B](implicit dec: Lazy[Decoder[A, B]]): Decoder[A, B @@ Id] =
    new Decoder[A, B @@ Id] {
      override def decode(a: A): DecodeResult[B @@ Id] = {
        // These are different types, so the compiler shouldn't try to satisfy `dec` with `this`, but it does!
        require(dec.value != this)
        dec.value.decode(a).map(autoWrap)
      }
    }
}

sealed trait Attribute extends Marker

object Attribute {
  implicit def autoWrap[A](a: A): A @@ Attribute = tag[Attribute](a)

  implicit def attributeTagDecoder[A, B](implicit dec: Lazy[Decoder[A, B]]): Decoder[A, B @@ Attribute] =
    dec.value.decode(_).map(autoWrap)
}

sealed trait Relationship extends Marker

object Relationship {
  implicit def autoWrap[A](a: A): A @@ Relationship = tag[Relationship](a)

  implicit def relationshipTagDecoder[A, B](implicit dec: Lazy[Decoder[A, B]]): Decoder[A, B @@ Relationship] =
    dec.value.decode(_).map(autoWrap)
}
