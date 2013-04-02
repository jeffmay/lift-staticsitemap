package net.liftmodules.staticsitemap

import org.scalatest.matchers.ShouldMatchers
import net.liftweb.sitemap.{Loc, NormalLocPath}
import org.scalatest.FunSpec
import net.liftweb.common.Full
import net.liftweb.sitemap.Loc.If

// TODO: Test url param escaping
// TODO: Test nested permissions

class TestSubRoutes extends FunSpec
with ShouldMatchers
with SubRouteBehaviors
{
  describe("A ParameterlessSubRoute") {
    val ParameterlessSiteMap = new StaticSiteMap {
      val default = new ParameterlessSubRoute("/bogus")
      val singleString = new ParameterlessSubRoute("bogus" -> "/custom")
      val singleList = new ParameterlessSubRoute(List("better") -> "/custom")
      val multiple = new ParameterlessSubRoute(List("multiple", "parts") -> "/custom")
    }

    it should behave like aDefaultUnitSubRoute(ParameterlessSiteMap.default)
    it should behave like allUnitSubRoutes(ParameterlessSiteMap.singleString)
    it should behave like allUnitSubRoutes(ParameterlessSiteMap.singleList)
    it should behave like allUnitSubRoutes(ParameterlessSiteMap.multiple)

    it should behave like aRelativeRouteBuilder((mapping: (List[PathPart], String)) => {
      val sm = new StaticSiteMap {
        val subroute = new ParameterlessSubRoute(mapping._1 -> mapping._2)
      }
      sm.subroute
    })
  }

  describe("The / factory") {
    val SlashSiteMap = new StaticSiteMap {
      val part = / ("a")
      val customPart = / ("a" -> "/b")
      val list = / (List("a", "b"))
      val customList = / (List("x", "y") -> "/z")
    }

    it should behave like aDefaultUnitSubRoute(SlashSiteMap.part)
    it should behave like aDefaultUnitSubRoute(SlashSiteMap.list)
    it should behave like allUnitSubRoutes(SlashSiteMap.list)
    it should behave like allUnitSubRoutes(SlashSiteMap.customList)

    it ("should produce a url from a single string with the url as the default template path") {
      SlashSiteMap.part.url should be ("/a")
      SlashSiteMap.part.templatePath should be ("/a")
    }

    it ("should accept a mapping from a single string to a custom template") {
      SlashSiteMap.customPart.url should be ("/a")
      SlashSiteMap.customPart.templatePath should be ("/b")
    }

    it ("should accept a mapping from a list of string with the url as the default template path") {
      SlashSiteMap.list.url should be ("/a/b")
      SlashSiteMap.list.templatePath should be ("/a/b")
    }

    it ("should accept a mapping from a list of string to a custom template") {
      SlashSiteMap.customList.url should be ("/x/y")
      SlashSiteMap.customList.templatePath should be ("/z")
    }

    it ("should prevent matching on a path containing a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = / ("/a/b/c")
        }
      } should produce [PathPartConstructionException]
    }

    it ("(when given a custom template path) should prevent matching on a path containing a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = / ("/a/b/c" -> "/custom")
        }
      } should produce [PathPartConstructionException]
    }

    it ("should prevent matching on a list of paths if any contains a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = / (List("a", "b/c"))
        }
      } should produce [PathPartConstructionException]
    }

    it ("(when given a custom template path) should prevent matching on a list of paths if any contains a slash") {
      evaluating {
        new StaticSiteMap {
          val invalid = / (List("a", "b/c") -> "/custom")
        }
      } should produce [PathPartConstructionException]
    }
  }

  describe("A SubRoute") {
    val ParamSiteMap = new StaticSiteMap {
      val nilPrefix = new StringParamUrl(Nil -> "/root") with StringParamTestable {
        def url(id: String) = / (id)

        protected def paramForUrl = {
          case / (id) => Full(id)
        }

        // only used for testing
        def param(fullUri: String) = paramForUrl(splitPath(fullUri))
      }
      val singlePrefix = new StringParamUrl("prefix" -> "/root") with StringParamTestable {
        def url(id: String) = / (id)

        protected def paramForUrl = {
          case / (id) => Full(id)
        }

        // only used for testing
        def param(fullUri: String) = paramForUrl(splitPath(fullUri))
      }
      val doublePrefix = new StringParamUrl(List("first", "second") -> "/root") with StringParamTestable {
        def url(id: String) = / (id)

        protected def paramForUrl = {
          case / (id) => Full(id)
        }

        // only used for testing
        def param(fullUri: String) = paramForUrl(splitPath(fullUri))
      }
    }
    ParamSiteMap.toSiteMap

    it should behave like aStringParamSubRoute(ParamSiteMap.nilPrefix)
    it should behave like aStringParamSubRoute(ParamSiteMap.singlePrefix)
    it should behave like aStringParamSubRoute(ParamSiteMap.doublePrefix)

    it ("should also be able to construct a url with a parameter and some constant value") {
      val ConstAndParamSiteMap = new StaticSiteMap {
        val sub = new StringParamUrl {
          def url(id: String) = / (id, "constant")

          protected def paramForUrl = {
            case / (id, "constant") => Full(id)
          }

          // only used for testing
          def param(fullUri: String) = paramForUrl(splitPath(fullUri))
        }
      }
      ConstAndParamSiteMap.sub.url("x") should be ("/x/constant")
      ConstAndParamSiteMap.sub.param("/x/constant") should be (Full("x"))
    }

    it ("should also be able to construct a url with multiple constants") {
      val ConstAndParamSiteMap = new StaticSiteMap {
        val sub = new StringParamUrl {
          def url(y: String) = / ("x", y, "z")

          protected def paramForUrl = {
            case / ("x", y, "z") => Full(y)
          }

          // only used for testing
          def param(fullUri: String) = paramForUrl(splitPath(fullUri))
        }
      }
      ConstAndParamSiteMap.sub.url("y") should be ("/x/y/z")
      ConstAndParamSiteMap.sub.param("/x/y/z") should be (Full("y"))
    }
  }

  describe("The :/ factory") {
    val SimpleRoutes = new StaticSiteMap {
      val plain = :/ ("/page")
      val tuple = :/ ("/customPage" -> "/customTemplate")
    }
    it should behave like aDefaultUnitSubRoute(SimpleRoutes.plain)
    it should behave like allUnitSubRoutes(SimpleRoutes.tuple)

    it ("should allow constructing root urls with custom template paths") {
      val SampleRoutes = new StaticSiteMap {
        val customPath = :/ ("/page1" -> "/customTemplate")
      }
      SampleRoutes.customPath.url should be ("/page1")
      SampleRoutes.customPath.toRoute should have ('path (List(NormalLocPath("customTemplate"))))
    }

    it ("should accept LocParams") {
      val NoAccess = If(() => false, Loc.strToFailMsg("Fogettaboughtit"))
      val SampleRoutes = new StaticSiteMap {
        val restricted = :/ ("/restricted", NoAccess)
      }
      SampleRoutes.restricted.params should be (List(NoAccess))
    }

    it should behave like anAbsoluteUrlRouteBuilder(SimpleRoutes :/ _)
  }
}
