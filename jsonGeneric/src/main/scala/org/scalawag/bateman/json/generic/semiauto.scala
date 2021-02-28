package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{Decoder, Encoder, JObject}
import org.scalawag.bateman.json.{Decoder, Encoder, JObject}
import shapeless.Lazy

object semiauto {
  def deriveDecoder[A](implicit decoder: Lazy[DerivedDecoder[A]]): Decoder[JObject, A] = decoder.value
  def deriveEncoder[A](implicit encoder: Lazy[DerivedEncoder[A]]): Encoder[A, JObject] = encoder.value
}
