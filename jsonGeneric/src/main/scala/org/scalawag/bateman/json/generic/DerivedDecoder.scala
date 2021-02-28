package org.scalawag.bateman.json.generic

import cats.data.NonEmptyChain
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.instances.option._
import shapeless.{
  :+:,
  ::,
  CNil,
  Coproduct,
  Default,
  Generic,
  HList,
  HNil,
  Inl,
  Inr,
  LabelledGeneric,
  Lazy,
  Poly1,
  Witness
}
import org.scalawag.bateman.json.{Decoder, JAny, JObject, UnexpectedField, UnspecifiedField}
import shapeless.labelled.field
import shapeless.HList._
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.ops.record.{Keys, Values}
import shapeless.labelled.FieldType
import shapeless.ops.coproduct.ToHList

trait DerivedDecoder[A] extends Decoder[JObject, A]

object DerivedDecoder {

  implicit def hnilDecoder(implicit config: DecoderConfig = DecoderConfig.default): DerivedDecoder[HNil] = { in =>
    if (config.allowExtraFields)
      HNil.validNec
    else
      NonEmptyChain.fromSeq(
        in.fields.map(_.name).map(UnexpectedField)
      ) match {
        case None     => HNil.validNec
        case Some(ee) => ee.invalid
      }
  }

  /** Generically decodes a JObject according to a record. If any of the labels (keys) in the record are not
    * represented in the JObject, it inserts `null` into the output record. This is used as a marker so that it
    * can later be reported and/or set to a default value.
    */
  implicit def hconsDecoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hDecoder: Lazy[Decoder[JAny, H]],
      tDecoder: DerivedDecoder[T],
      config: DecoderConfig = DecoderConfig.default
  ): DerivedDecoder[FieldType[K, Option[H]] :: T] = { in =>
    val fieldName = config.fieldNameMapping(witness.value.name)

    // If a matching field exists, attempt to decode its value.
    val hr = in.optionalField(fieldName).map(hDecoder.value.decode).sequence

    // Strip the matching field out of the object for the tail decoder. (Consume this field.)
    val objRemains = in - fieldName

    // Attempt to decode the tail.
    val tr = tDecoder.decode(objRemains)

    // Combine the results of decoding the head with the results of decoding the tail.
    (hr, tr).mapN { case (h, t) => field[K](h) :: t }
  }

  object optionize extends Poly1 {
    implicit def genericCase[K <: Symbol, H]: Case.Aux[FieldType[K, H], FieldType[K, Option[H]]] =
      at(a => field[K](Option(a)))
  }

  implicit def genericDecoder[
      // The case class for which we're generating a Decoder
      CaseClass,
      // The default labelled representation of this class as an HList
      Labelled <: HList,
      // The default unlabelled representation of this class as an HList
      Unlabelled <: HList,
      // The labelled representation with each of the values wrapped in an Option
      UnlabelledOptional <: HList,
      // The unlabelled representation with each of the values wrapped in an Option
      LabelledOptional <: HList
  ](implicit
      // Generate the Labelled representation from CaseClass
      labelledGeneric: LabelledGeneric.Aux[CaseClass, Labelled],
      // Generate the LabelledOptional representation by mapping Labelled through optionize
      optionizer: Mapper.Aux[optionize.type, Labelled, LabelledOptional],
      // Generate the Unlabelled representation from the CaseClass
      generic: Generic.Aux[CaseClass, Unlabelled],
      // Get the default parameters of the case class, which happens to appear as an UnlabelledOptional
      defaults: Default.AsOptions.Aux[CaseClass, UnlabelledOptional],
      // Generate the coalesced, unlabelled constructor arguments
      fallbackToDefaults: FallbackToDefaults.Aux[LabelledOptional, UnlabelledOptional, Unlabelled],
      // Generate a decoder to use for the LabelledOptional representation. This is how we'll process the JSON.
      decoder: DerivedDecoder[LabelledOptional],
      // How we get configuration information into the generated decoder. This is the only user-supplied implicit.
      config: DecoderConfig = DecoderConfig.default
  ): DerivedDecoder[CaseClass] = { in =>
    // Invoke the generated HList decoder to get back a record containing the constructor arguments that could be
    // extracted from the JSON. This will fail if there are problems. Not finding one of the expected fields is
    // _not_ a problem (yet). When a value is found in the JSON and decoded, it will appear in a Some here. If no
    // corresponding JSON field is found, it will appear as a None.
    decoder.decode(in).andThen { optionalDecodedArguments =>
      val optionalDefaultArguments = defaults()
      // Attempt to coalesce the decoded values with those in the defaults.
      fallbackToDefaults(optionalDecodedArguments, optionalDefaultArguments)
        .map(generic.from)
        .leftMap { ee =>
          ee.map(e => UnspecifiedField(in, config.fieldNameMapping(e)))
        }
    }
  }

  implicit def cnilDecoder(implicit config: DecoderConfig = DecoderConfig.default): DerivedDecoder[CNil] = { in =>
    val discriminatorField = config.discriminatorField
    in.field(discriminatorField).andThen(_.asString).andThen { f =>
      // We have no visibility into the valid discriminators here. All we know is that none of the coproductDecoders
      // managed to find a discriminator that matched it. So, just return the error without the valids and we'll
      // transform it further up the stack to insert the valids into it.
      InvalidDiscriminator(f, Nil).invalidNec
    }
  }

  implicit def coproductDecoder[K <: Symbol, H, T <: Coproduct](implicit
      witness: Witness.Aux[K],
      hDecoder: Lazy[DerivedDecoder[H]],
      tDecoder: DerivedDecoder[T],
      config: DecoderConfig = DecoderConfig.default
  ): DerivedDecoder[FieldType[K, H] :+: T] = { in =>
    val discriminatorField = config.discriminatorField
    val typeName = config.classNameMapping(witness.value.name)
    in.field(discriminatorField).andThen(_.asString).map(_.s).andThen { t =>
      if (t == typeName) {
        // When we pass this on to the concrete (hlist) decoder, remove the discriminator field.
        // It won't be expecting it.
        hDecoder.value.decode(in - discriminatorField).map(field[K](_)).map(Inl(_))
      } else
        tDecoder.decode(in).map(Inr(_))
    }
  }

  implicit def traitDecoder[A, R <: Coproduct, H <: HList, K <: HList](implicit
      gen: LabelledGeneric.Aux[A, R],
      toHList: ToHList.Aux[R, H],
      keys: Keys.Aux[H, K],
      keyTrav: ToTraversable.Aux[K, List, Symbol],
      decoder: DerivedDecoder[R],
      config: DecoderConfig = DecoderConfig.default
  ): DerivedDecoder[A] = { in =>
    decoder.decode(in).map(gen.from).leftMap { ee =>
      ee.map {
        // If we detected a bad discriminator down in the cnilDecoder, actually fill it out with the valid
        // discriminators here.
        case disc: InvalidDiscriminator =>
          disc.copy(valids = keyTrav(keys()).map(_.name).map(config.classNameMapping))
        case e => e
      }
    }
  }
}
