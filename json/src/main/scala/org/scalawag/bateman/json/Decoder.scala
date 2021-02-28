package org.scalawag.bateman.json

import cats.data.NonEmptyChain
import cats.syntax.validated._
import cats.syntax.apply._
import cats.syntax.traverse._

import java.util.UUID

trait Decoder[-A, B] {
  def decode(a: A): DecodeResult[B]

  def map[C](fn: B => C): Decoder[A, C] = { a =>
    decode(a).map(fn)
  }

  def flatMap[C](that: Decoder[B, C]): Decoder[A, C] = { a =>
    decode(a).andThen(that.decode)
  }
}

object Decoder {
  def apply[A, B](implicit decoder: Decoder[A, B]): Decoder[A, B] = decoder

  def decode[A, B](a: A)(implicit decoder: Decoder[A, B]): DecodeResult[B] = decoder.decode(a)

  implicit def identityDecoder[A]: Decoder[A, A] = _.validNec

  implicit def optionDecoder[A](implicit
      dec: Decoder[JAny, A]
  ): Decoder[JAny, Option[A]] = {
    case _: JNull =>
      None.validNec
    case jany =>
      dec.decode(jany).map(Some.apply)
  }

  implicit def mapDecoder[A, B](implicit
      adec: Decoder[JString, A],
      bdec: Decoder[JAny, B]
  ): Decoder[JAny, Map[A, B]] =
    _.asObject.andThen { obj =>
      // Make sure we don't have multiple fields with the same name (which is surprisingly allowed by the JSON spec).
      val duplicateNames =
        obj.fields.map(_.name).groupBy(_.s).filter(_._2.size > 1).values
      val duplicateNameErrors = duplicateNames.map { dn =>
        val positions =
          dn.flatMap(_.lineColumnOption).map(_.toString).mkString(", ")
        // TODO: make gets safe! vvvvvv
        GenericSchemaError(
          obj.lineColumnOption,
          obj.pointerOption,
          s"object has multiple fields named '${dn.head.s}' ($positions)"
        )
      }
      NonEmptyChain.fromSeq(duplicateNameErrors.toSeq) match {
        case Some(ee) =>
          ee.invalid
        case None =>
          obj.fields
            .map { f =>
              (adec.decode(f.name), bdec.decode(f.value)).tupled
            }
            .toList
            .sequence[DecodeResult, (A, B)]
            .map(_.toMap)
      }
    }

  implicit def listDecoder[A](implicit
      dec: Decoder[JAny, A]
  ): Decoder[JAny, List[A]] =
    _.asArray.andThen { arr =>
      arr.arr.map(dec.decode).toList.sequence[DecodeResult, A]
    }

  implicit val jAnyDecoder: Decoder[JAny, JAny] = _.validNec
  implicit val jnullDecoder: Decoder[JAny, JNull] = _.asNull
  implicit val jarrayDecoder: Decoder[JAny, JArray] = _.asArray
  implicit val jobjectDecoder: Decoder[JAny, JObject] = _.asObject
  implicit val jstringDecoder: Decoder[JAny, JString] = _.asString
  implicit val jnumberDecoder: Decoder[JAny, JNumber] = _.asNumber
  implicit val jbooleanDecoder: Decoder[JAny, JBoolean] = _.asBoolean

  implicit val stringDecoder: Decoder[JAny, String] = _.asString.map(_.s)
  implicit val booleanDecoder: Decoder[JAny, Boolean] = _.asBoolean.map(_.b)

  private def safeConvert[A <: JAny, B](fn: A => B)(a: A): DecodeResult[B] =
    try {
      fn(a).validNec
    } catch {
      case ex: IllegalArgumentException =>
        InvalidValue(a, ex.getMessage).invalidNec
    }

  implicit val uuidDecoder: Decoder[JAny, UUID] =
    _.asString.andThen(safeConvert { js =>
      UUID.fromString(js.s)
    })

  implicit val intDecoder: Decoder[JAny, Int] = _.asNumber.andThen(safeConvert { jn =>
    jn.n.toInt
  })
}
