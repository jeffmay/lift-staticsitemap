package net.liftmodules.staticsitemap

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{TestFailedException, FunSpec}

trait RouteContainerBehaviors {
  this: FunSpec with ShouldMatchers =>

  def aRoutesContainer(container: RoutesContainer[_]) {
    it("(%s) should not have a url method".format(container.toString)) {
      intercept[TestFailedException] {
        container should not (have ('url("_")))
      }
    }
  }

  def aChildRouteInSiteMap(parent: StaticSiteMap)(child: ConvertibleToRoute[_]) {
    val childRoute = child.toRoute

    it should behave like aChildRouteOf(parent)(child)

    it("(%s) should add itself to the containing StaticSiteMap %s".format(childRoute.name, parent)) {
      assert(
        parent.toSiteMap.findLoc(childRoute.name).isDefined,
        "SiteMap %s should contain the added %s".format(parent, childRoute)
      )
    }
  }

  def aChildRouteOf(parent: RoutesContainer[_])(child: ConvertibleToRoute[_]) {
    val childRoute = child.toRoute
    it("(%s) should add itself to the parent pane page %s routes".format(childRoute.name, parent)) {
      assert(
        parent.routes exists {_.name == childRoute.name},
        "Enclosing PanePages should contain the added %s".format(childRoute)
      )
    }

    it("(%s) should inherit the loc params from the containing pane page %s".format(childRoute.name, parent)) {
      parent.prefixParams foreach { pageParam =>
        assert(childRoute.params contains pageParam, "%s does not have %s".format(childRoute, pageParam))
      }
    }

    parent.parent match {
      case Some(sitemap: StaticSiteMap) =>
        it should behave like aChildRouteInSiteMap(sitemap)(child)
      case Some(parent: RoutesContainer[_]) =>
        it should behave like aChildRouteOf(parent)(child)
      case None =>
    }
  }

}
