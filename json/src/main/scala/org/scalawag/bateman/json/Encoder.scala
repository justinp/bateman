package org.scalawag.bateman.json

trait Encoder[-A, +B] {
  def encode(a: A): B

  def contramap[C](fn: C => A): Encoder[C, B] = { c =>
    encode(fn(c))
  }
}

object Encoder {
  def apply[A, B](implicit encoder: Encoder[A, B]): Encoder[A, B] = encoder

  def encode[A, B](a: A)(implicit encoder: Encoder[A, B]): B = encoder.encode(a)

  implicit val stringEncoder: Encoder[String, JString] = JString(_)
  implicit val booleanEncoder: Encoder[Boolean, JBoolean] = JBoolean(_)
  implicit val intEncoder: Encoder[Int, JNumber] = n => JNumber(n.toString)
  implicit val doubleEncoder: Encoder[Double, JNumber] = n => JNumber(n.toString)
  implicit val floatEncoder: Encoder[Float, JNumber] = n => JNumber(n.toString)
}
