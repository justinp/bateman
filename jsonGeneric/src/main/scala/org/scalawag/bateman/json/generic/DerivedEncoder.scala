package org.scalawag.bateman.json.generic

import org.scalawag.bateman.json.{Encoder, JAny, JField, JObject, JString}
import org.scalawag.bateman.json.{Encoder, JAny, JObject}
import shapeless.{:+:, ::, CNil, Coproduct, Default, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Align, Diff, Intersection, Prepend, ToTraversable}

import scala.annotation.implicitNotFound

trait DerivedEncoder[A] extends Encoder[A, JObject]

object DerivedEncoder {

  implicit val hnilEncoder: DerivedEncoder[HNil] = { _ => JObject(Nil) }

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[Encoder[H, JAny]],
      tEncoder: DerivedEncoder[T],
      config: EncoderConfig = EncoderConfig.default
  ): DerivedEncoder[FieldType[K, H] :: T] = { in =>
    val fieldName = config.fieldNameMapping(witness.value.name)
    val obj = tEncoder.encode(in.tail)
    if (in.head == null) // Indicates that this value was removed because it equalled the default value.
      obj
    else
      JObject(JField(JString(fieldName), hEncoder.value.encode(in.head)) +: obj.fields)
  }

  implicit def genericEncoder[
      A, // the case class we're encoding
      R <: HList, // the generic representation (HList) of the case class
      D <: HList, // the HList of the fields that have default parameters
      Diff <: HList, // the HList of the fields that don't have default parameters
      Unaligned <: HList // a transitory HList of the fields after reassembly, before realigning
  ](implicit
      labelledGeneric: LabelledGeneric.Aux[A, R],
      defaults: Default.AsRecord.Aux[A, D],
      selectFieldsWithDefaults: Intersection.Aux[R, D, D],
      selectFieldsWithoutDefaults: Diff.Aux[R, D, Diff],
      removeRedundantValues: RemoveRedundancy.Aux[D, D],
      prepend: Prepend.Aux[Diff, D, Unaligned],
      realign: Align[Unaligned, R],
      trav: ToTraversable.Aux[R, List, AnyRef],
      encoder: DerivedEncoder[R],
      config: EncoderConfig = EncoderConfig.default
  ): DerivedEncoder[A] = { in =>
    // Create the generic representation of our instance to encode.
    val gen = labelledGeneric.to(in)

    // See if any of the field values are `null`. If so, throw a NPE. We're going to use `null` as a marker later to
    // indicate that a field should not be incuded in the output JObject, so having the caller pass in a `null` value
    // muddies that whole strategy. They shouldn't be using `null` anyway.
    if (gen.toList.contains(null))
      throw new NullPointerException

    // Find the fields from the instance that have defaults by intersecting it with the defaults type.
    val instanceFieldsWithDefaults = selectFieldsWithDefaults(gen)

    // Set any fields to `null` that have the same value as their default. We know that they are going to have the
    // same type, because we derived them both from the case class. The defaults will be a subset of the full
    // representation and they should already be in the same order.
    val instanceFieldsWithNulls = removeRedundantValues(instanceFieldsWithDefaults, defaults())

    // Now, get all the instance fields that _don't_ have any defaults.
    val instanceFieldsWithoutDefaults = selectFieldsWithoutDefaults(gen)

    // Put our possibly-nulled fields with the ones that didn't have defaults (and so couldn't be set to null).
    // The order may be messed up here, depending on the positioning of the default parameters. For example, if the
    // last field is the only one without a default parameter, this will effectively move it to the front.
    val unorderedFieldsWithNulls = prepend(instanceFieldsWithoutDefaults, instanceFieldsWithNulls)

    // Reorder (align) the fields to the full representation of our case class. This is just so that they appear in
    // the JSON with the order that they have in the case class.
    val orderedFieldsWithNulls = realign(unorderedFieldsWithNulls)

    // Call the encoder using the possibly-nulled fields. It's designed to exclude fields with null values.
    encoder.encode(orderedFieldsWithNulls)
  }

  implicit val cnilEncoder: DerivedEncoder[CNil] = { _ => ??? }

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[DerivedEncoder[H]],
      tEncoder: DerivedEncoder[T],
      config: EncoderConfig = EncoderConfig.default
  ): DerivedEncoder[FieldType[K, H] :+: T] = {
    case Inl(h) =>
      val discriminatorField = config.discriminatorField
      val concreteClassName = witness.value.name
      val discriminatorValue = config.classNameMapping(concreteClassName)
      val encoded = hEncoder.value.encode(h)
      // Detect whether the discriminator field is also a field added by the subclass encoder.
      // TODO: It would be great to make this a compile-time check. I'm not sure how, given the fact that it can be
      //       set at runtime. One option is to force the discriminator field to "type" and not let the programmer
      //       customize it. Also, once we're returning runtime errors from Encoders, we really need to have a
      //       Validated return type. Do we really want to bite that off just for this?
      if (encoded.getField(discriminatorField).nonEmpty)
        throw DiscriminatorFieldCollision(discriminatorField, concreteClassName)
      encoded + (discriminatorField -> JString(discriminatorValue))
    case Inr(t) =>
      tEncoder.encode(t)
  }

  implicit def traitEncoder[A, R <: Coproduct](implicit
      gen: LabelledGeneric.Aux[A, R],
      encoder: DerivedEncoder[R]
//      config: EncoderConfig = EncoderConfig.default
  ): DerivedEncoder[A] = { in =>
    encoder.encode(gen.to(in))
  }
}
