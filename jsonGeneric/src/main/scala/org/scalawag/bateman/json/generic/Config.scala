package org.scalawag.bateman.json.generic

trait Config {
  val fieldNameMapping: String => String
  val classNameMapping: String => String
  val discriminatorField: String
}

case class DecoderConfig(
    fieldNameMapping: String => String = identity,
    classNameMapping: String => String = identity,
    discriminatorField: String = "type",
    useDefaults: Boolean = true,
    allowExtraFields: Boolean = true
) extends Config

object DecoderConfig {
  val default: DecoderConfig = DecoderConfig()
}

case class EncoderConfig(
    fieldNameMapping: String => String = identity,
    classNameMapping: String => String = identity,
    discriminatorField: String = "type"
//    useDefaults: Boolean = true // TODO: don't encode a field if it's set to its default value
) extends Config

object EncoderConfig {
  val default: EncoderConfig = EncoderConfig()
}
