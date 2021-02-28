package org.scalawag.bateman.json.generic

import cats.syntax.validated._
import cats.data.{NonEmptyChain, ValidatedNec}
import shapeless._
import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.ops.record.{Keys, Values}

/** Given a labelled HList that contains Optional values and an unlabelled HList that contains the same types, return
  * either the coalesced values as an unlabelled HList _without_ the Option wrappers or a list of the keys for which
  * no value could be determined. Values are processed by position. If there is a value in the first (labelled) HList,
  * that is the output value. Otherwise, if there is a value in the second (unlabelled) HList, that is the output.
  * If there is not a value in either HList, that causes an Invalid to be returned. The labels of all such values are
  * included in the Invalid result. The output HList contains only the values with no labels.
  */

trait FallbackToDefaults[LabelledOptional] {
  type AbstractUnlabelledOptional
  type AbsractUnlabelled

  def apply(decoded: LabelledOptional, defaults: AbstractUnlabelledOptional): ValidatedNec[String, AbsractUnlabelled]
}

object FallbackToDefaults {
  type Aux[LabelledOptional, UnlabelledOptional, Unlabelled] = FallbackToDefaults[LabelledOptional] {
    type AbstractUnlabelledOptional = UnlabelledOptional
    type AbsractUnlabelled = Unlabelled
  }

  implicit def hlistCoalescence[
      // The labelled decoded values, each wrapped in an Option
      LabelledOptioned <: HList,
      // The unlabelled defaults, each wrapped in an Option
      UnlabelledOptioned <: HList,
      // The keys (field names) extracted from the labelled decoded values (L)
      Keys <: HList,
      // The unlabelled values, without the Option wrappers
      UnlabelledUnoptioned <: HList
  ](implicit
      // To extract the field names (keys) from the first (labelled) HList
      keys: Keys.Aux[LabelledOptioned, Keys],
      // To turn Keys into a List of Symbols
      keyTrav: ToTraversable.Aux[Keys, List, Symbol],
      // To coalesce the two HLists
      coalescer: Coalesce[UnlabelledOptioned],
      // To remove the labels from the convert the results of the nested decoder from labelled to unlabelled
      values: Values.Aux[LabelledOptioned, UnlabelledOptioned],
      // to unwrap the coalesced ctor args
      deoptionizer: Mapper.Aux[
        deoptionize.type,
        UnlabelledOptioned,
        UnlabelledUnoptioned
      ],
      // to turn Option-wrapped ctor args into a List of Options
      argsTrav: ToTraversable.Aux[UnlabelledOptioned, List, Option[
        Any
      ]],
      config: DecoderConfig = DecoderConfig.default
  ): FallbackToDefaults.Aux[LabelledOptioned, UnlabelledOptioned, UnlabelledUnoptioned] =
    new FallbackToDefaults[LabelledOptioned] {
      override type AbstractUnlabelledOptional = UnlabelledOptioned
      override type AbsractUnlabelled = UnlabelledUnoptioned

      override def apply(
          decoded: LabelledOptioned,
          defaults: AbstractUnlabelledOptional
      ): ValidatedNec[String, AbsractUnlabelled] = {
        // Extract just the values from the decoded record. Throw away the keys.
        val decodedArgsWithNulls = values(decoded)

        // Now, wrap all the args in Options. That means that the `null`s will become Nones. Everything else is a Some.
        val decodedArgsAsOptions = decodedArgsWithNulls // optionizer.apply(decodedArgsWithNulls)

        // Coalesce the decoded args with the defaults (which are already also Options), if the configuration allows.
        val coalescedArgs =
          if (config.useDefaults)
            coalescer.apply(decodedArgsAsOptions, defaults)
          else
            decodedArgsAsOptions

        // At this point, we should have a Some for every constructor arg -- either from the JSON or from the case
        // class defaults. If we don't, those are errors, which we're detecting here. Since everything is positional
        // now (we stripped the labels), we'll zip the field names with the coalesced arguments so that we can tell
        // which fields we're missing.
        val errors = argsTrav(coalescedArgs).zip(keyTrav(keys())).collect {
          case (None, fieldName) => fieldName
        }

        // If we have any errors, report them. Otherwise, perform a technically unsafe, deoptionize. If anything in
        // coalescedArgs is a None, this will throw, but we made sure none of them were empty above. Putting the
        // error-handling logic in here means that we don't have to try to pass the context (name and object) down
        // into deoptionize. Finally, use our generic to instantiate the case class.
        NonEmptyChain.fromSeq(errors) match {
          case Some(ee) => ee.map(_.name).invalid
          case None     => deoptionizer.apply(coalescedArgs).validNec
        }
      }
    }
}
