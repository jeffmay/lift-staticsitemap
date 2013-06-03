package net.liftmodules.staticsitemap

import net.liftweb.mockweb.WebSpec
import net.liftweb.http.LiftRules
import net.liftweb.sitemap.Loc.Template
import net.liftweb.common.Full

object SampleSiteMap extends StaticSiteMap {
  val valid = <b>valid</b>
  val validTemplate = Template(() => valid)

  val Index = @/(^, validTemplate)
  val Const = @/("const", validTemplate)
}

class RootIndexTest extends WebSpec(
  () => {
    LiftRules.setSiteMap(SampleSiteMap.toSiteMap)
  }
) {

  import SampleSiteMap._

  "The SiteMap" should {
    "- not resolve a non existant url" withReqFor("/empty") in {
      _.location.isEmpty
    }
  }

  "Root Index" should {

    "- be a root menu" in {
      Index.toRoute.toMenu.isRoot_?
    }

    "- resolve the / url" withReqFor("/") in {
      _.location.isDefined
    }

    "- resolve the / url with Index menu" withReqFor("/") in {
      req => Index.toRoute.toMenu.findLoc(req).isDefined
    }

    "- should resolve a template" withReqFor("/") in {
      req => {
        req.location.get.template must_== Full(valid)
      }
    }

  }
}
