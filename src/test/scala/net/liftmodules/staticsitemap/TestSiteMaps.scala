package net.liftmodules.staticsitemap

import net.liftweb.mockweb.WebSpec
import net.liftweb.http.LiftRules

object SampleSiteMap extends StaticSiteMap {
  val Index = @/(^)
}

class RootIndexTest extends WebSpec(
  () => {
    LiftRules.setSiteMap(SampleSiteMap.toSiteMap)
  }
) {

  "SiteMap Root Index" should {
    "Resolve the / url" withReqFor("/") in {
      _.location.isDefined
    }
  }
}
