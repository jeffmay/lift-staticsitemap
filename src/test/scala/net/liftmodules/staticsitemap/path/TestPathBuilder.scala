package net.liftmodules.staticsitemap.path

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.sitemap.*

class TestPathBuilder extends FunSpec with ShouldMatchers {

  describe("PathBuilder") {

    describe("when extracting path parts") {

      describe("with %") {

        it("should behave like ^** when given no arguments") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / "one"
            def extract = {
              case extracted @ % () => extracted
            }
          }
          builder.extract(^ / "one") should be (builder.^** : List[String])
        }

        it("should not accept zero parameters when a wildcard is present in the parts") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / *
            def extract = {
              case % () => Nil
            }
          }
          assert(!builder.extract.isDefinedAt(^ / "param"),
            "Should not match missing wildcard."
          )
        }

        it("should not accept a constant and zero parameters when a wildcard is present in the parts") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / "const" / *
            def extract = {
              case % () => Nil
            }
          }
          assert(!builder.extract.isDefinedAt(^ / "const" / "param"),
            "Should not match missing wildcard."
          )
        }

        it("should fill a single wildcard") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / *
            def extract = {
              case % (param) => List(param)
            }
          }
          builder.extract(List("one")) should be (List("one"))
        }

        it("should fill a single wildcard followed by a constant") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / * / "const"
            def extract = {
              case % (param) => List(param)
            }
          }
          builder.extract(List("one", "const")) should be (List("one"))
        }

        it("should fill a single wildcard preceded by a constant") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / "const" / *
            def extract = {
              case % (param) => List(param)
            }
          }
          builder.extract(List("const", "one")) should be (List("one"))
        }

        it("should fill a single wildcard in between two constants") {
          val builder = new PathBuilder with PathExtracting {
            def parts = ^ / "first" / * / "last"
            def extract = {
              case % (param) => List(param)
            }
          }
          builder.extract(List("first", "middle", "last")) should be (List("middle"))
        }

      }
    }
  }
}
