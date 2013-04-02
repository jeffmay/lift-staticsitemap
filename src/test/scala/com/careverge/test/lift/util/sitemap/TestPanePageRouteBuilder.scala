package com.careverge.test.lift.util.sitemap

import com.careverge.lift.util.sitemap.PanePageRoutesBuilder
import org.scalatest.{FunSpec, TestFailedException}
import org.scalatest.matchers.ShouldMatchers
import net.liftmodules.staticsitemap.{SubRouteBehaviors, StaticSiteMap}

// TODO: Test defaults

class TestPanePageRouteBuilder extends FunSpec
with ShouldMatchers
with SubRouteBehaviors
{
  describe("PanePageRoute") {
    val PanePages = new StaticSiteMap with PanePageRoutesBuilder {
      val page = new PanePageRoute("page") {
        val paneUrl = / ("url")
        val defaultPaneUrl = / ("defaulturl")
        val pane = new / ("pane") {
          val subPaneUrl = / ("url")
          val subPane = new / ("subpane") {
            val subSubPaneUrl = / ("url")
          }
        }
        override val Default = defaultPaneUrl
      }
      val hasNoSnippetData = new PanePageRoute("nodata_nosnippetdata") {
        val pane = / ("pane")
        override val Default = pane
      }
      val hasNoSnippetSubData = new PanePageRoute("data_nosubdata") {
        val pane = new / ("pane") {
          val subPane = / ("data")
        }
        override val Default = pane.subPane
      }
      val hasSnippetSubData = new PanePageRoute("data_subdata") {
        val pane = new / ("pane") {
          val subPane = new / ("data") {
            val subDataUrl = / ("subdata")
          }
        }
        override val Default = pane.subPane.subDataUrl
      }
    }
    PanePages.toSiteMap

    it should behave like allSubRoutes(PanePages.page)
//    TODO: pane pages should inherit common functionality to sub routes from a single type
//    it should behave like aDefaultUnitSubRoute(PanePages.page.paneUrl)

    describe("when resolving parameterless urls") {
      it ("should allow url resolution to a pane with no data") {
        PanePages.page.paneUrl.url should be ("/page/url")
      }
      it ("should not allow url resolution to a pane") {
        intercept[TestFailedException] {
          PanePages.page.pane should not have ('url ("_"))
        }
      }
      it ("should allow url resolution to a sub-pane URL with no data") {
        PanePages.page.pane.subPaneUrl.url should be ("/page/pane/url")
      }
      it ("should not url resolution to a sub-pane") {
        intercept[TestFailedException] {
          PanePages.page.pane.subPane should not have ('url ("_"))
        }
      }
      it ("should allow url resolution to a sub-sub-pane URL with no data") {
        PanePages.page.pane.subPane.subSubPaneUrl.url should be ("/page/pane/subpane/url")
      }
    }

    describe ("(when matching data in urls)") {
      it ("should properly configure pane page with no data and no subdata") {
        assert(!PanePages.hasNoSnippetData.hasSnippetData, "PanePage has data when it shouldn't")
        assert(!PanePages.hasNoSnippetData.hasSnippetSubData, "PanePage has sub data when it shouldn't")
      }
      it ("should properly configure pane page with data but no subdata") {
        assert(PanePages.hasNoSnippetSubData.hasSnippetData, "PanePage doesn't have data when it should")
        assert(!PanePages.hasNoSnippetSubData.hasSnippetSubData, "PanePage has sub data when it shouldn't")
      }
      it ("should properly configure pane page with data and subdata") {
        assert(PanePages.hasSnippetSubData.hasSnippetData, "PanePage doesn't have data when it should")
        assert(PanePages.hasSnippetSubData.hasSnippetSubData, "PanePage has sub data when it shouldn't")
      }
    }

    describe("(when picking a default url)") {
      it ("should choose the correct default templatePath") {
        PanePages.page.defaultTemplate should be (PanePages.page.Default.templateFile)
      }
      it ("should resolve urls to the default pane without specifying the template file in the url") {
        PanePages.page.Default.url should be ("/page")
        PanePages.page.defaultPaneUrl.url should be ("/page")
      }
    }

    describe("A ParamPaneUrl") {
      it ("should return a pane url after the parameters are applied to calculate the url") (pending)
    }
  }
}
