package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{Decoder, Encoder, JObject}
import org.scalawag.bateman.json.{Decoder, Encoder, JObject}
import shapeless.Lazy

object auto {
  implicit def deriveDecoder[A](implicit decoder: Lazy[DerivedDecoder[A]]): Decoder[JObject, A] = semiauto.deriveDecoder
  implicit def deriveEncoder[A](implicit encoder: Lazy[DerivedEncoder[A]]): Encoder[A, JObject] = semiauto.deriveEncoder
}
