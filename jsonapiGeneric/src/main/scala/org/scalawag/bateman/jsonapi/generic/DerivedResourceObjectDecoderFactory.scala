package org.scalawag.bateman.jsonapi.generic

import cats.syntax.apply._
import cats.syntax.validated._
import org.scalawag.bateman.json.generic.DerivedDecoder.optionize
import org.scalawag.bateman.json.generic.FallbackToDefaults
import org.scalawag.bateman.json.{Decoder, JAny, JString, UnspecifiedField}
import org.scalawag.bateman.jsonapi.{ResourceObject, Relationship => JRelationship}
import shapeless.labelled.{FieldType, field}
import shapeless._
import shapeless.ops.hlist.Mapper
import shapeless.tag.{@@, Tagged}

trait DerivedResourceObjectDecoderFactory[A] {
  def apply(resourceType: String): Decoder[ResourceObject, A]
}

object DerivedResourceObjectDecoderFactory {
  def apply[A](implicit decoder: DerivedResourceObjectDecoderFactory[A]): DerivedResourceObjectDecoderFactory[A] =
    decoder

  implicit def hnilDecoder[A]: DerivedResourceObjectDecoderFactory[HNil] = { _ => _ => HNil.validNec }

  implicit def hconsIdDecoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hDecoder: Lazy[Decoder[JString, H]],
      tailDecoder: DerivedResourceObjectDecoderFactory[T],
      config: DecoderConfig = DecoderConfig.default
  ): DerivedResourceObjectDecoderFactory[FieldType[K, Option[H @@ Id]] :: T] = { resourceType => in =>
    val fieldName = config.fieldNameMapping(witness.value.name)
    require(fieldName == "id")
    (
      in.optionalIdAs[H](hDecoder.value),
      tailDecoder(resourceType).decode(in)
    ).mapN { (h, t) =>
      field[K](h.map(tag[Id](_))) :: t
    }
  }

  implicit def hconsAttributeDecoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hDecoder: Lazy[Decoder[JAny, H]],
      tailDecoder: DerivedResourceObjectDecoderFactory[T],
      config: DecoderConfig = DecoderConfig.default
  ): DerivedResourceObjectDecoderFactory[FieldType[K, Option[H with Tagged[Attribute]]] :: T] = { resourceType => in =>
    val fieldName = config.fieldNameMapping(witness.value.name)
    (
      in.optionalAttributeAs[H](fieldName)(hDecoder.value),
      tailDecoder(resourceType).decode(in)
    ).mapN { (h, t) =>
      field[K](h.map(tag[Attribute](_))) :: t
    }
  }

  implicit def hconsRelationshipDecoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hDecoder: Lazy[Decoder[JRelationship, H]],
      tailDecoder: DerivedResourceObjectDecoderFactory[T],
      config: DecoderConfig = DecoderConfig.default
  ): DerivedResourceObjectDecoderFactory[FieldType[K, Option[H @@ Relationship]] :: T] = { resourceType => in =>
    val fieldName = config.fieldNameMapping(witness.value.name)
    (
      in.optionalRelationshipAs[H](fieldName)(hDecoder.value),
      tailDecoder(resourceType).decode(in)
    ).mapN { (h, t) =>
      field[K](h.map(tag[Relationship](_))) :: t
    }
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
      decoder: Lazy[DerivedResourceObjectDecoderFactory[LabelledOptional]],
      // How we get configuration information into the generated decoder. This is the only user-supplied implicit.
      config: DecoderConfig = DecoderConfig.default
  ): DerivedResourceObjectDecoderFactory[CaseClass] = { resourceType => in =>
    // Invoke the generated HList decoder to get back a record containing the constructor arguments that could be
    // extracted from the JSON. This will fail if there are problems. Not finding one of the expected fields is
    // _not_ a problem (yet). When a value is found in the JSON and decoded, it will appear in a Some here. If no
    // corresponding JSON field is found, it will appear as a None.
    decoder.value(resourceType).decode(in).andThen { optionalDecodedArguments =>
      val optionalDefaultArguments = defaults()
      // Attempt to coalesce the decoded values with those in the defaults.
      fallbackToDefaults(optionalDecodedArguments, optionalDefaultArguments)
        .map(generic.from)
        .leftMap { ee =>
          ee.map(e =>
            // TODO: This is wrong. Distinguish between attr and rel.
            UnspecifiedField(in.src, config.fieldNameMapping(e))
          )
        }
    }
  }

}
