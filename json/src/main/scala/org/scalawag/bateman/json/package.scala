package org.scalawag.bateman

import cats.data.{NonEmptyChain, ValidatedNec}

package object json {
  type DecodeResult[+A] = ValidatedNec[SchemaError, A]

  def formatErrorReport(errors: NonEmptyChain[SchemaError]): String =
    sortErrors(errors).map(_.fullDescription).mkString(" - ", "\n - ", "\n")

  // Remove duplicates and sort in a reasonable order (by position).
  def sortErrors(errors: NonEmptyChain[SchemaError]): Iterable[SchemaError] = {
    errors.iterator.toList.sortBy(x => x.lineColumn).distinct
  }
}
