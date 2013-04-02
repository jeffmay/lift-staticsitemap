package com.careverge.test.lift.client.util

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JObject

class TestCustomSerializer extends FunSpec with ShouldMatchers with GivenWhenThen {

  describe("ComposerSerializer") {

    trait CommonTrait {
      def x: String
      def y: String
    }
    case class ClassOfA(a: String, x: String, y: String) extends CommonTrait
    case class ClassOfB(b: String, x: String, y: String) extends CommonTrait

    case class Composer(kind: String, composed: CommonTrait)

    object ComposerSerializer extends CustomSerializer[Composer](
      implicit format => (
        {
          case JObject(JField("kind", JString(kind)) :: JField("composed", composed: JObject) :: Nil) =>
            kind match {
              case "A" => Composer(kind, composed.extract[ClassOfA])
              case "B" => Composer(kind, composed.extract[ClassOfB])
            }
        },
        {
          case model: Composer => Extraction.decompose(model)
        }
      )
    )

    implicit val formats = DefaultFormats + ComposerSerializer

    // A //
    val A = JObject(List(
      JField("a", "team"),
      JField("x", "Mr."),
      JField("y", "T")
    ))
    val composerOfA = JObject(List(
      JField("kind", "A"),
      JField("composed", A)
    ))
    val modelA = ClassOfA("team", "Mr.", "T")
    val modelComposerOfA = Composer("A", modelA)

    // B //
    val B = JObject(List(
      JField("b", "east"),
      JField("x", "wars"),
      JField("y", "yo")
    ))
    val composerOfB = JObject(List(
      JField("kind", "B"),
      JField("composed", B)
    ))
    val modelB = ClassOfB("east", "wars", "yo")
    val modelComposerOfB = Composer("B", modelB)

    describe("when extracting a trait") {
      it ("should be able to extract the composed class A") {
        val extracted = A.extract[ClassOfA]
        extracted should be (modelA)
      }

      it ("should be able to extract the composed class B") {
        val extracted = B.extract[ClassOfB]
        extracted should be (modelB)
      }

      it ("should extract the correct type composed class from the serialized model of A") {
        val sampleJson = Serialization.write(A)
        val sample = Serialization.read[ClassOfA](sampleJson)
        sample should be (modelA)
      }

      it ("should extract the correct type composed class from the serialized model of B") {
        val sampleJson = Serialization.write(B)
        val sample = Serialization.read[ClassOfB](sampleJson)
        sample should be (modelB)
      }

      it ("should not be able to extract the incorrect composed class for A") {
        intercept[MappingException] {
          (composerOfA \ "composed").extract[ClassOfB]
        }
      }

      it ("should not be able to extract the incorrect composed class for B") {
        intercept[MappingException] {
          (composerOfB \ "composed").extract[ClassOfA]
        }
      }
    }

    describe("when extracting a trait-composing class") {
      it ("should extract the correct type composed class from the composer of A") {
        val sampleJson = Serialization.write(composerOfA)
        val sample = Serialization.read[Composer](sampleJson)
        sample should be (modelComposerOfA)
      }

      it ("should extract the correct type composed class from the composer of B") {
        val sampleJson = Serialization.write(composerOfB)
        val sample = Serialization.read[Composer](sampleJson)
        sample should be (modelComposerOfB)
      }
    }

  }

  describe("CompositeSerializer") {

    trait Composite {
      def kind: String
      def score: Int
    }

    case class CompositeA(score: Int, other: String) extends {
      val kind = "A"
    } with Composite
    case class CompositeB(score: Int, other: Int) extends {
      val kind = "B"
    } with Composite


    case class Composer(name: String, composed: Composite)

    object CompositeSerializer extends CustomSerializer[Composite](
      implicit format => (
        {
          case composite@JObject(JField("kind", JString("A")) :: _) => composite.extract[CompositeA]
          case composite@JObject(JField("kind", JString("B")) :: _) => composite.extract[CompositeB]
        },
        {
          case model: Composite => Extraction.decompose(model)
        }
      )
    )

    implicit val formats = DefaultFormats + CompositeSerializer

    // A //
    val A = JObject(List(
      JField("kind", "A"),
      JField("score", 1),
      JField("other", "things")
    ))
    val composerOfA = JObject(List(
      JField("name", "composer of A"),
      JField("composed", A)
    ))
    val modelA = CompositeA(1, "things")
    val modelComposerOfA = Composer("composer of A", modelA)

    // B //
    val B = JObject(List(
      JField("kind", "B"),
      JField("score", -1),
      JField("other", 10)
    ))
    val composerOfB = JObject(List(
      JField("name", "composer of B"),
      JField("composed", B)
    ))
    val modelB = CompositeB(-1, 10)
    val modelComposerOfB = Composer("composer of B", modelB)

    describe("when extracting a trait") {
      it ("should be able to extract the composed class A") {
        val extracted = A.extract[CompositeA]
        extracted should be (modelA)
      }

      it ("should be able to extract the composed class B") {
        val extracted = B.extract[CompositeB]
        extracted should be (modelB)
      }

      it ("should extract the correct type composed class from the serialized model of A") {
        val sampleJson = Serialization.write(A)
        val sample = Serialization.read[CompositeA](sampleJson)
        sample should be (modelA)
      }

      it ("should extract the correct type composed class from the serialized model of B") {
        val sampleJson = Serialization.write(B)
        val sample = Serialization.read[CompositeB](sampleJson)
        sample should be (modelB)
      }

      it ("should not be able to extract the incorrect composed class for A") {
        intercept[MappingException] {
          // the serializer only cares about the kind, extraction failure is caused by a lack of conversion String => Int
          A.extract[CompositeB]
        }
      }

      it ("will allow extracting the incorrect composed class for B") {
        // this happens whether you use preinitialization or a key word argument (with or without a default)
        object strange extends CompositeA(-1, "10") {
          override val kind = "A"
        }
        val theResult = B.extract[CompositeA]
        // the serializer only cares about the kind, extraction succeeds because of a conversion of Int => String
        theResult should be (strange)
      }
    }

    describe("when extracting a trait-composing class") {
      it ("should extract the correct type composed class from the composer of A") {
        val sampleJson = Serialization.write(composerOfA)
        val sample = Serialization.read[Composer](sampleJson)
        sample should be (modelComposerOfA)
      }

      it ("should extract the correct type composed class from the composer of B") {
        val sampleJson = Serialization.write(composerOfB)
        val sample = Serialization.read[Composer](sampleJson)
        sample should be (modelComposerOfB)
      }
    }
  }

}
