package net.liftmodules.staticsitemap

import org.scalatest.{TestFailedException, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.liftweb.common.Full
import net.liftweb.sitemap.{SiteMapException, Menu, SiteMap}
import net.liftweb.sitemap.Loc.LocParam

class TestRoutesBuilder extends FunSpec with ShouldMatchers with SubRouteBehaviors {

  case object LocParamA extends LocParam[Any]
  case object LocParamB extends LocParam[Any]
  case object LocParamC extends LocParam[Any]
  case object LocParamD extends LocParam[Any]

  trait NestedRoutes extends RoutesBuilder {
    val single = / ("single")
    val paramUrl = new StringParamUrl(Nil -> "/paramurl") {
      protected def paramForUrl = {
        case / (param) => Full(param)
      }
      def url(param: String) = / (param)
    }
    val prefixedParamUrl = new StringParamUrl("prefix" -> "/paramTemplate") {
      protected def paramForUrl = {
        case / (param) => Full(param)
      }
      def url(param: String) = / (param)
    }
    val nested = new / ("nested", LocParamA, LocParamB) {
      val inner = / ("inner")
      val custom = / ("custom" -> "/customTemplate")
      val paramUrl = new StringParamUrl(Nil -> "/paramurl") {
        protected def paramForUrl = {
          case / (param) => Full(param)
        }
        def url(param: String) = / (param)
      }
      val prefixedParamUrl = new StringParamUrl("prefix" -> "/paramTemplate") {
        protected def paramForUrl = {
          case / (param) => Full(param)
        }
        def url(param: String) = / (param)
      }
      val appendLocParamC = / ("c", LocParamC)
      val appendLocParamCandD = / ("c" :: "d" :: Nil, LocParamC, LocParamD)
    }
  }
  val NestedRoutes = new StaticSiteMap with NestedRoutes {}

  describe("A RoutesBuilder") {

    it ("should allow conversion to a sitemap with prefixed parameterized urls in the same container as a non-prefixed one") {
      assert(NestedRoutes.toSiteMap != null, "Resulting SiteMap was null")
    }

    it ("should add its routes to its enclosing sitemap") {
      val test = Route("test", "url", "/template", "test")
      val Enclosing = new StaticSiteMap {
        val builder = new / {}
      }
      Enclosing.builder.addToRoutes(test)
      assert(
        Enclosing.routes contains test,
        "Enclosing sitemap should contain the added sub route"
      )
    }

    describe("when including routes from an exterior RoutesBuilder") {
      val ExteriorRoutes = new NestedRoutes {}
      val MainSiteMap = new StaticSiteMap {
        val external = new / with NestedRoutes
      }

      it ("should add parameterless routes") {
        val single = ExteriorRoutes.single.toRoute
        assert(MainSiteMap.routes exists { _.name == single.name },
          "Missing exterior route."
        )
      }

      it ("should add parameterized routes") {
        val paramurl = ExteriorRoutes.paramUrl.toRoute
        assert(MainSiteMap.routes exists { _.name == paramurl.name },
          "Missing parameterized exterior route."
        )
      }

      it ("should add nested parameterless routes") {
        val inner = ExteriorRoutes.nested.inner.toRoute
        assert(MainSiteMap.routes exists { _.name == inner.name },
          "Missing nested exterior route."
        )
      }

      it ("should add nested parameterized routes") {
        val paramurl = ExteriorRoutes.nested.paramUrl.toRoute
        assert(MainSiteMap.routes exists { _.name == paramurl.name },
          "Missing nested parameterized exterior route."
        )
      }
    }

    describe("when nesting routes inside a Routes container") {
      it ("should not allow calling url on the Routes container") {
        evaluating {
          NestedRoutes.nested should not have ('url ("_"))
        } should produce [TestFailedException]
      }

      it ("should prefix nested paremeterless routes' urls with the route container's prefix") {
        NestedRoutes.nested.inner.url should be ("/nested/inner")
      }

      it ("should prefix nested paremeterless prefixed routes' urls with the route container's prefix") {
        NestedRoutes.nested.custom.url should be ("/nested/custom")
      }

      it ("should prefix nested parameterized routes' urls with the route container's prefix") {
        NestedRoutes.nested.paramUrl.url("something") should be ("/nested/something")
      }

      it ("should prefix nested parameterized prefixed routes' urls with the route container's prefix") {
        NestedRoutes.nested.prefixedParamUrl.url("something") should be ("/nested/prefix/something")
      }

      it ("should prefix nested parameterless routes' names with the route container's prefix") {
        NestedRoutes.nested.inner.name should be ("/nested/inner")
      }

      it ("should prefix nested parameterless prefixed routes' names with the route container's prefix") {
        NestedRoutes.nested.custom.name should be ("/nested/custom")
      }

      it ("should prefix nested parameterized routes' names with the route container's prefix") {
        NestedRoutes.nested.paramUrl.name should be ("/nested/**")
      }

      it ("should prefix nested parameterized prefixed routes' names with the route container's prefix") {
        NestedRoutes.nested.prefixedParamUrl.name should be ("/nested/prefix/**")
      }

      it ("should add the route container's loc params") {
        NestedRoutes.nested.inner.toRoute.toMenu.loc.params should be (
          LocParamA :: LocParamB :: Nil)
      }

      it ("should prepend its loc params to a route's loc param") {
        NestedRoutes.nested.appendLocParamC.toRoute.toMenu.loc.params should be (
          LocParamA :: LocParamB :: LocParamC :: Nil)
      }

      it ("should prepend its loc params to a route's multiple loc params") {
        NestedRoutes.nested.appendLocParamCandD.toRoute.toMenu.loc.params should be (
          LocParamA :: LocParamB :: LocParamC :: LocParamD :: Nil)
      }
    }
  }

  describe("The StaticSiteMap") {

    it ("should not allow two routes with the same url and same template path") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = / ("duplicate")
        val duplicate = / ("duplicate")
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two routes with the same prefix and different template paths") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = / ("duplicate" -> "/template1")
        val duplicate = / ("duplicate" -> "/template2")
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two nested routes with the same prefix and different template paths") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new / ("duplicate") {
          val inside = / ("inside" -> "/template1")
        }
        val duplicate = new / ("duplicate") {
          val inside = / ("inside" -> "/template2")
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two nested routes with the same prefix list and different template paths") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new / ("duplicate") {
          val inside = / (List("same", "inside") -> "/template1")
        }
        val duplicate = new / ("duplicate") {
          val inside = / (List("same", "inside") -> "/template2")
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two default parameterized routes without any prefix or template path") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new StringParamUrl {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
        val duplicate = new StringParamUrl {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two parameterized routes without any prefix and the same template") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new StringParamUrl(Nil -> "/same") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
        val duplicate = new StringParamUrl(Nil -> "/same") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two nested parameterized routes with no prefix or template path") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new / ("duplicate") {
          val inside = new StringParamUrl {
            protected def paramForUrl = {
              case / (param) => Full(param)
            }

            def url(param: String) = / (param)
          }
        }
        val duplicate = new / ("duplicate") {
          val inside = new StringParamUrl {
            protected def paramForUrl = {
              case / (param) => Full(param)
            }

            def url(param: String) = / (param)
          }
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two nested parameterized routes without any prefix and different templates") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new StringParamUrl(Nil -> "/template1") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
        val duplicate = new StringParamUrl(Nil -> "/template2") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two nested parameterized routes with the same prefix and template paths") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new StringParamUrl("prefix") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
        val duplicate = new StringParamUrl("prefix") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    it ("should not allow two nested parameterized routes with the same prefix and different template paths") {
      val DuplicateSiteMap = new StaticSiteMap {
        val original = new StringParamUrl("prefix" -> "/template1") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
        val duplicate = new StringParamUrl("prefix" -> "/template2") {
          protected def paramForUrl = {
            case / (param) => Full(param)
          }

          def url(param: String) = / (param)
        }
      }
      evaluating { DuplicateSiteMap.toSiteMap } should produce [SiteMapException]
    }

    describe("when added to a Lift SiteMenu") {
      it ("should be able to add static routes after a site menu") {
        val SampleSiteMap = SiteMap(
          Menu("appended", "Appended") / "appended" / "after" >> NestedRoutes.AddStaticRoutesAfter,
          Menu("last", "Last") / "last"
        )
        val sitemap = NestedRoutes.sitemapMutator(SampleSiteMap)
        sitemap.kids.head.loc.name should be ("appended")
        sitemap.kids.tail.zip(NestedRoutes.routes).foreach {
          case (menu: Menu, route: Route[_]) =>
            menu.loc.name should be (route.name)
        }
        sitemap.kids.last.loc.name should be ("last")
      }

      it ("should be able to substitute static routes for a site menu") {
        val SampleSiteMap = SiteMap(
          Menu("substituted", "Substituted") / "replaced" / "here" >> NestedRoutes.AddStaticRoutesHere,
          Menu("last", "Last") / "last"
        )
        val sitemap = NestedRoutes.sitemapMutator(SampleSiteMap)
        sitemap.kids.zip(NestedRoutes.routes).foreach {
          case (menu: Menu, route: Route[_]) =>
            menu.loc.name should be (route.name)
        }
        sitemap.kids.last.loc.name should be ("last")
      }

      it ("should be able to add static routes before a site menu") {
        val SampleSiteMap = SiteMap(
          Menu("under", "Under") / "inserted" / "below" >> NestedRoutes.AddStaticRoutesUnder,
          Menu("last", "Last") / "last"
        )
        val sitemap = NestedRoutes.sitemapMutator(SampleSiteMap)
        val parent = sitemap.kids.head
        parent.loc.name should be ("under")
        parent.kids.zip(NestedRoutes.routes).foreach {
          case (menu: Menu, route: Route[_]) =>
            menu.loc.name should be (route.name)
        }
        sitemap.kids.last.loc.name should be ("last")
      }
    }
  }
}
