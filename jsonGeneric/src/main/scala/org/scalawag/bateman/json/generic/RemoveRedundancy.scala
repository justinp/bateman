package org.scalawag.bateman.json.generic

import shapeless._

trait RemoveRedundancy[R <: HList] {
  type Out <: HList

  def apply(r: R, o: Out): R
}

object RemoveRedundancy {
  final type Aux[R <: HList, Out0 <: HList] = RemoveRedundancy[R] { type Out = Out0 }

  implicit final val hnilRemoveRedundancy: Aux[HNil, HNil] =
    new RemoveRedundancy[HNil] {
      final type Out = HNil

      final def apply(r: HNil, o: HNil): HNil = HNil
    }

  implicit final def hconsRemoveRedundancy[K <: Symbol, V, T <: HList](implicit
      tailProcessor: RemoveRedundancy[T]
  ): Aux[V :: T, V :: tailProcessor.Out] =
    new RemoveRedundancy[V :: T] {
      final type Out = V :: tailProcessor.Out

      final def apply(
          r: V :: T,
          o: V :: tailProcessor.Out
      ): V :: T = {
        if (o.head == r.head)
          null.asInstanceOf[V] :: tailProcessor(r.tail, o.tail)
        else
          r.head :: tailProcessor(r.tail, o.tail)
      }
    }
}
