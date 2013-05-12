package net.liftmodules.staticsitemap

import path._
import path.PathUtils._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

trait RouteConverterBehaviors extends ShouldMatchers {
  this: FunSpec =>

  def allSubRoutes(routable: ConvertibleToRoute[_]) {
    it ("(%s) should equal a route with the same values".format(routable.name)) {
      routable.toRoute should be === (routable.toRoute)
    }
  }

  def aStringParamSubRoute(routable: ConvertibleToRoute1[String]) {
    it ("(%s) should have a name that is the combined full path of its name parts".format(routable.name)) {
      val fullNamePath: String = routable.nameParts.asFullPath
      routable.name should be (fullNamePath)
    }

    it ("(%s) should be able to add a parameter to a url".format(routable.name)) {
      val fullPathWithX: String = mkFullPath(routable.urlPathParts.parts.map{_.slug} :+ "x")
      routable.url("x") should be (fullPathWithX)
    }

    it ("(%s) should be able to retrieve a parameter from a url".format(routable.name)) {
      val fullPathWithX: List[String] = (routable.urlPathParts.parts.map{_.slug} :+ "x").toList
      routable.paramForUrl(fullPathWithX) should be ("x")
    }

    it should behave like allSubRoutes(routable)
  }

  def allUnitSubRoutes(routable: ConvertibleToRoute0) {
    it ("(%s) should have the same name as its url".format(routable.name)) {
      routable.name should be (routable.url)
    }

    it should behave like allSubRoutes(routable)
  }

  def aDefaultUnitSubRoute(routable: ConvertibleToRoute0) {
    it ("(%s) should have the same url defined as the default loc href".format(routable.name)) {
      val route = routable.toRoute
      route.toMenu.loc.calcDefaultHref should be (routable.url)
    }

    it should behave like allUnitSubRoutes(routable)
  }

  def aRelativeRouteBuilder[R <: ConvertibleToRoute[_]](build: ((PathParts, String)) => R) {
    it ("(when only given a template path) should not accept template paths without a leading /") {
      evaluating {
        new StaticSiteMap {
          val badUrl = build(^ -> "wrong")
        }
      } should produce [PathPartSplitException]
    }

    it ("(when given url path parts and template path) should not accept template paths without a leading /") {
      evaluating {
        new StaticSiteMap {
          val badTemplatePath = build("okay" -> "wrong")
        }
      } should produce [PathPartSplitException]
    }

    it ("(when given url path parts and a template path) should not accept url paths with a leading /") {
      evaluating {
        new StaticSiteMap {
          val badUrlParts = build("/not/valid" -> "/okay")
        }
      } should produce [PathPartConstructionException]
    }

    it ("(when given a template path) should accept a valid template path") {
      assert(new StaticSiteMap {
        val goodTemplatePath = build(^ -> "/okay")
      } != null, "SiteMap was null")
    }

    it ("(when given a url path part string and a template path) should accept a valid url part and template path") {
      assert(new StaticSiteMap {
        val okayUrl = build("satisfactory" -> "/okay")
      } != null, "SiteMap was null")
    }

    it ("(when given url path parts and a template path) should accept a valid list of url parts and template path") {
      assert(new StaticSiteMap {
        val okayUrl = build(^ / "better" / "option" -> "/okay")
      } != null, "SiteMap was null")
    }
  }
}
