package org.scalawag.bateman.jsonapi.generic

import org.scalawag.bateman.json.{Decoder, JAny, formatErrorReport}
import org.scalawag.bateman.json.parser.JsonParser
import org.scalawag.bateman.jsonapi.{Document, ResourceObject}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import shapeless.Lazy
import shapeless.tag.@@

import java.util.UUID

class DerivedResourceObjectDecoderFactoryTest extends AnyFunSpec with Matchers {
  final case class MyClass(
      id: UUID @@ Id
  )

  object MyClass {
    implicit def decoder: Decoder[ResourceObject, MyClass] = semiauto.deriveDecoder("my_class")
  }

  it("gets a stack overflow") {
    val in = JsonParser.parse("""
        {
          "data" : {
            "type" : "my_class",
            "id" : "55555555-5555-5555-5555-555555555555"
          }
        }
      """).andThen(_.as[Document]).andThen(_.datum).fold(ee => fail(ee.toString), identity)

    val decoded = Decoder[ResourceObject, MyClass].decode(in)
    val res = decoded.fold(formatErrorReport, identity)
    println(res)
  }

  it("is essentially what it's doing, though this doesn't compile!") {
    assertDoesNotCompile {
      "val d: Decoder[JAny, UUID @@ Id] = Id.idTagDecoder(Lazy(Id.idTagDecoder(Decoder.uuidDecoder)))"
    }
  }
}
