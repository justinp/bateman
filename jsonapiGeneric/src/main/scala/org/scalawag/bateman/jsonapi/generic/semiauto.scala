package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.{Decoder, JObject}
import org.scalawag.bateman.jsonapi.{ResourceIdentifier, ResourceObject}
import shapeless.Lazy

object semiauto {
  def deriveDecoder[A](
      resourceType: String
  )(implicit decoder: Lazy[DerivedResourceObjectDecoderFactory[A]]): Decoder[ResourceObject, A] =
    decoder.value(resourceType)
}
