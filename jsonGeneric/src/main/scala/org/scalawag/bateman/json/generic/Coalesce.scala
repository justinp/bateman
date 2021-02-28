package org.scalawag.bateman.json.generic

import shapeless._

trait Coalesce[R <: HList] {
  def apply(r: R, o: R): R
}

object Coalesce {
  implicit final val hnilFallbackToDefaults: Coalesce[HNil] = { (_, _) => HNil }

  implicit final def hconsFallbackToDefaults[K <: Symbol, V, T <: HList](implicit
      tailCoalesce: Coalesce[T]
  ): Coalesce[Option[V] :: T] = { (first, second) =>
    (first.head orElse second.head) :: tailCoalesce(first.tail, second.tail)
  }
}
