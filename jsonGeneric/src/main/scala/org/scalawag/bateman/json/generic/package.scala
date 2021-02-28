package org.scalawag.bateman.json

import shapeless.Poly1
import shapeless.labelled.{FieldType, field}

package object generic {

  object deoptionize extends Poly1 {
    implicit def genericCase[A]: Case.Aux[Option[A], A] = at(_.get)
  }

}
