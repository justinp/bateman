package org.scalawag.bateman.jsonapi.generic

case class DecoderConfig(
    fieldNameMapping: String => String = identity,
    classNameToResourceType: String => String = identity,
    useDefaults: Boolean = true
)

object DecoderConfig {
  val default: DecoderConfig = DecoderConfig()
}
