package net.liftmodules.staticsitemap

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import net.liftweb.sitemap.Loc.LocParam
import net.liftweb.common.Full
import net.liftweb.sitemap.NormalLocPath
import path._
import path.PathBuilder._

// TODO: Test url param escaping

class TestSubRoutes extends FunSpec
with ShouldMatchers
with RouteConverterBehaviors {

  describe("A ParameterlessSubRoute") {
    val ParameterlessSiteMap = new StaticSiteMap {
      val default = new ParameterlessSubRoute("/bogus")
      val singleString = new ParameterlessSubRoute("custom" -> "/custom")
      val singleList = new ParameterlessSubRoute(^ / "better" -> "/better")
      val multiple = new ParameterlessSubRoute(^ / "multiple" / "parts" -> "/multiple/parts")
    }

    it should behave like aDefaultUnitSubRoute(ParameterlessSiteMap.default)
    it should behave like allUnitSubRoutes(ParameterlessSiteMap.singleString)
    it should behave like allUnitSubRoutes(ParameterlessSiteMap.singleList)
    it should behave like allUnitSubRoutes(ParameterlessSiteMap.multiple)

    it should behave like aRelativeRouteBuilder(
      (mapping: (PathParts, String)) => {
        val sm = new StaticSiteMap {
          val subroute = new ParameterlessSubRoute(mapping._1 -> mapping._2)
        }
        sm.subroute
      })
  }

  describe("Relative Snails") {
    val SlashSiteMap = new StaticSiteMap {
      val part = @/("a")
      val customPart = @/("b" -> "/c")
      val list = @/(List("a", "b"))
      val customList = @/(^ / "x" / "y" -> "/z")
    }

    it should behave like aDefaultUnitSubRoute(SlashSiteMap.part)
    it should behave like allUnitSubRoutes(SlashSiteMap.customPart)
    it should behave like aDefaultUnitSubRoute(SlashSiteMap.list)
    it should behave like allUnitSubRoutes(SlashSiteMap.customList)

    it("should produce a url from a single string with the url as the default template path") {
      SlashSiteMap.part.url should be("/a")
      SlashSiteMap.part.templatePath should be("/a")
    }

    it("should accept a mapping from a single string to a custom template") {
      SlashSiteMap.customPart.url should be("/b")
      SlashSiteMap.customPart.templatePath should be("/c")
    }

    it("should accept a mapping from a list of string with the url as the default template path") {
      SlashSiteMap.list.url should be("/a/b")
      SlashSiteMap.list.templatePath should be("/a/b")
    }

    it("should accept a mapping from a list of string to a custom template") {
      SlashSiteMap.customList.url should be("/x/y")
      SlashSiteMap.customList.templatePath should be("/z")
    }

    it("should prevent matching on a path containing a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = @/("/a/b/c")
        }
      } should produce[PathPartConstructionException]
    }

    it("(when given a custom template path) should prevent matching on a path containing a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = @/("/a/b/c" -> "/custom")
        }
      } should produce[PathPartConstructionException]
    }

    it("should prevent matching on a list of paths if any contains a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = @/(^ / "a" / "b/c")
        }
      } should produce[PathPartConstructionException]
    }

    it("(when given a custom template path) should prevent matching on a list of paths if any contains a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = @/(^ / "a" / "b/c" -> "/custom")
        }
      } should produce[PathPartConstructionException]
    }
  }

  describe("A SubRoute") {

    val ParamSiteMap = new StaticSiteMap {
      val nilPrefix = new String_@/(^ -> "/root") {
        def url(id: String) = ^** / id

        def paramForUrl = {
          case ^** / id => Full(id)
          case what => {
            val thing = what
            val shouldbe = ^**
            Full(thing.toString)
          }
        }
      }
      val singlePrefix = new String_@/("prefix" -> "/root") {
        def url(id: String) = ^** / id

        def paramForUrl = {
          case ^** / id => Full(id)
        }
      }
      val doublePrefix = new String_@/(^** / "first" / "second" -> "/root") {
        def url(id: String) = ^** / id

        def paramForUrl = {
          case ^** / param => Full(param)
        }
      }
    }
    ParamSiteMap.toSiteMap

    it should behave like aStringParamSubRoute(ParamSiteMap.nilPrefix)
    it should behave like aStringParamSubRoute(ParamSiteMap.singlePrefix)
    it should behave like aStringParamSubRoute(ParamSiteMap.doublePrefix)

    it("should also be able to construct a url with a parameter and some constant value") {
      val ConstAndParamSiteMap = new StaticSiteMap {
        val sub = new String_@/ {
          def url(id: String) = ^** / id / "constant"

          def paramForUrl = {
            case ^** / id / "constant" => Full(id)
          }
        }
      }
      ConstAndParamSiteMap.sub.url("x") should be("/x/constant")
      ConstAndParamSiteMap.sub.paramForUrl(^ / "x" / "constant") should be(Full("x"))
    }

    it("should also be able to construct a url with multiple constants") {
      val ConstAndParamSiteMap = new StaticSiteMap {
        val sub = new String_@/ {
          def url(y: String) = ^** / "x" / y / "z"

          def paramForUrl = {
            case ^** / "x" / y / "z" => Full(y)
          }
        }
      }
      ConstAndParamSiteMap.sub.url("y") should be("/x/y/z")
      ConstAndParamSiteMap.sub.paramForUrl(^ / "x" / "y" / "z") should be(Full("y"))
    }
  }

  describe("Fixed Urls") {
    case object ParamA extends LocParam[Any]
    case object ParamB extends LocParam[Any]

    val SimpleRoutes = new StaticSiteMap {
      val plain = :/("/page")
      val tuple = :/("/customPage" -> "/customTemplate")
      val restricted = :/("/restricted", ParamA)
      val container = new @/("wrong", ParamA) {
        val absolute = :/("/absolute", ParamB)
      }
    }
    it should behave like aDefaultUnitSubRoute(SimpleRoutes.plain)
    it should behave like allUnitSubRoutes(SimpleRoutes.tuple)

    it("should allow constructing root urls with custom template paths") {
      val SampleRoutes = new StaticSiteMap {
        val customPath = :/("/page1" -> "/customTemplate")
      }
      SampleRoutes.customPath.url should be("/page1")
      SampleRoutes.customPath.toRoute should have('path(List(NormalLocPath("customTemplate"))))
    }

    it("should accept LocParams") {
      SimpleRoutes.restricted.locParams should be(List(ParamA))
    }

    it("^ should be /index.html") {
      val sitemap = new StaticSiteMap {
        val Home = :/(^)
      }
      sitemap.Home.templatePath should be ("/index")
    }

    it("should prevent constructing root urls without a leading /") {
      evaluating {
        new StaticSiteMap {
          val noSlash = :/("wrong" -> "wrong")
        }
      } should produce[PathPartSplitException]
    }

    it("should prevent constructing template paths without a leading /") {
      evaluating {
        new StaticSiteMap {
          val noSlash = :/("/okay" -> "wrong")
        }
      } should produce[PathPartSplitException]
    }

    it("should prevent constructing root urls with empty path parts (ie. //)") {
      evaluating {
        new StaticSiteMap {
          val noSlash = :/("/not/a/good//url" -> "/okay")
        }
      } should produce[PathPartConstructionException]
    }

    it("should prevent constructing template paths with empty path parts (ie. //)") {
      evaluating {
        new StaticSiteMap {
          val noSlash = :/("/a/fine/url" -> "/not/a/good//template/path")
        }
      } should produce[PathPartConstructionException]
    }

    it("should not contain the parent container's prefix") {
      SimpleRoutes.container.absolute.url should be("/absolute")
    }

    it("should not contain the parent container's loc param") {
      SimpleRoutes.container.absolute.locParams should be(List(ParamB))
    }
  }
}
