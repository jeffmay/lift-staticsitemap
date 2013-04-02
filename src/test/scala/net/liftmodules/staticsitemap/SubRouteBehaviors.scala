package net.liftmodules.staticsitemap

import PathUtils._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.common.Box

trait SubRouteBehaviors extends ShouldMatchers {
  this: FunSpec =>

  trait StringParamTestable {
    def url(param: String): String
    def param(fullUri: String): Box[String]
  }

  def allSubRoutes(routable: ConvertibleToRoute[_]) {
    it ("(%s) should equal a route with the same values".format(routable.name)) {
      routable.toRoute should be === (routable.toRoute)
    }
  }

  def aStringParamSubRoute(routable: RoutesBuilder#SubRoute[String] with StringParamTestable) {
    it ("(%s) should have the same name as its path root followed by a \"/**\"".format(routable.name)) {
      val fullPathWithStar = mkFullPath(routable.pathParts ::: "**" :: Nil)
      routable.name should be (fullPathWithStar)
    }

    it ("(%s) should be able to add a parameter to a url".format(routable.name)) {
      val fullPathWithX = mkFullPath(routable.pathParts ::: "x" :: Nil)
      routable.url("x") should be (fullPathWithX)
    }

    it ("(%s) should be able to retrieve a parameter from a url".format(routable.name)) {
      val fullPathWithX = mkFullPath(routable.pathParts ::: "x" :: Nil)
      routable.param(fullPathWithX) should be ("x")
    }

    it should behave like allSubRoutes(routable)
  }

//  def aDefaultParamSubRoute[T](routable: RoutesBuilder#SubRoute[T]) {
//  }

  // TODO: Open this up to all parameterless urlws
  def allUnitSubRoutes(routable: RoutesBuilder#ParameterlessSubRoute) {
    it ("(%s) should have the same name as its url".format(routable.name)) {
      routable.name should be (routable.url)
    }

    it should behave like allSubRoutes(routable)
  }

  // TODO: Open this up to all parameterless urls
  def aDefaultUnitSubRoute(routable: RoutesBuilder#ParameterlessSubRoute) {
    it ("(%s) should use the given URL as the path to the template by default".format(routable.name)) {
      val route = routable.toRoute
      route.toMenu.loc.calcDefaultHref should be (route.url)
    }
  }

  def aRelativeRouteBuilder[T <: RoutesBuilder#SubRouteUrl](build: ((List[PathPart], String)) => T) {
    it ("(when only given a template path) should not accept template paths without a leading /") {
      evaluating {
        new StaticSiteMap {
          val badUrl = new ParameterlessSubRoute("wrong")
        }
      } should produce [PathPartSplitException]
    }

    it ("(when given url path parts and template path) should not accept template paths without a leading /") {
      evaluating {
        new StaticSiteMap {
          val badTemplatePath = new ParameterlessSubRoute("okay" -> "wrong")
        }
      } should produce [PathPartSplitException]
    }

    it ("(when given url path parts and a template path) should not accept url paths with a leading /") {
      evaluating {
        new StaticSiteMap {
          val badUrlParts = new ParameterlessSubRoute("/not/valid" -> "/okay")
        }
      } should produce [PathPartConstructionException]
    }

    it ("(when given a template path) should accept a valid template path") {
      assert(new StaticSiteMap {
        val goodTemplatePath = new ParameterlessSubRoute("/okay")
      } != null, "SiteMap was null")
    }

    it ("(when given a url path part string and a template path) should accept a valid url part and template path") {
      assert(new StaticSiteMap {
        val okayUrl = new ParameterlessSubRoute("satisfactory" -> "/okay")
      } != null, "SiteMap was null")
    }

    it ("(when given url path parts and a template path) should accept a valid list of url parts and template path") {
      assert(new StaticSiteMap {
        val okayUrl = new ParameterlessSubRoute(("better" :: "option" :: Nil) -> "/okay")
      } != null, "SiteMap was null")
    }
  }

  def anAbsoluteUrlRouteBuilder[T <: RoutesBuilder#SubRouteUrl](build: ((String, String)) => T)(implicit mf: Manifest[T]) {
    val name = mf.erasure

    it ("(when given url path parts and a template path) should accept valid url path parts and template path") {
      new StaticSiteMap {
        val okayUrl = new ParameterlessSubRoute(List("better") -> "/okay")
      }
    }

    it ("(%s) should prevent constructing root urls without a leading /".format(name)) {
      evaluating {
        new StaticSiteMap {
          val noSlash = build("wrong" -> "wrong")
        }
      } should produce [PathPartSplitException]
    }

    it ("(%s) should prevent constructing template paths without a leading /".format(name)) {
      evaluating {
        new StaticSiteMap {
          val noSlash = build("/okay" -> "wrong")
        }
      } should produce [PathPartSplitException]
    }

    it ("(%s) should prevent constructing root urls with empty path parts //".format(name)) {
      evaluating {
        new StaticSiteMap {
          val noSlash = build("/not/a/good//url" -> "/okay")
        }
      } should produce [PathPartSplitException]
    }

    it ("(%s) should prevent constructing template paths with empty path parts //".format(name)) {
      evaluating {
        new StaticSiteMap {
          val noSlash = build ("/a/fine/url" -> "/not/a/good//template/path")
        }
      } should produce [PathPartSplitException]
    }
  }
}
