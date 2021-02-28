package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.{Decoder, Encoder, JObject}
import org.scalawag.bateman.jsonapi.ResourceObject
import shapeless.Lazy

import scala.reflect.ClassTag

object auto {
  implicit def deriveDecoder[A](implicit
      classTag: ClassTag[A],
      decoder: Lazy[DerivedResourceObjectDecoderFactory[A]]
  ): Decoder[ResourceObject, A] =
    semiauto.deriveDecoder(classTag.runtimeClass.getSimpleName)
}
