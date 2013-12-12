package net.liftmodules.staticsitemap

import net.liftweb.mockweb.WebSpec
import net.liftweb.http._
import net.liftweb.common.Box
import scala.xml.NodeSeq

object SampleSiteMap extends StaticSiteMap {

  val Index = @/(^)
  val Const = @/("const")
}

class RootIndexTest extends WebSpec(
  () => {
    LiftRules.setSiteMap(SampleSiteMap.toSiteMap)
  }
) {

  /**
   * Cribbed from LiftSession and removed the request object.
   * @param path a request parse path
   * @return
   */
  def findVisibleTemplate(path: ParsePath): Box[NodeSeq] = {
    val tpath = path.partPath
    val splits = tpath.toList.filter {
      a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1
    } match {
      case Nil => List("index")
      case s => s
    }
    Templates(splits, S.locale)
  }

  import SampleSiteMap._

  /*
    All templates are located in the test webapp directory.
   */

  "The SiteMap" should {
    "not resolve a non existant url" withReqFor "/empty" in {
      _.location.isEmpty
    }
  }

  "Root Index" should {

    "be a root menu" in {
      Index.toRoute.toMenu.isRoot_?
    }

    "resolve the / url" withReqFor "/" in {
      _.location.isDefined
    }

    "resolve to the static Index loc" withReqFor "/" in {
      req => Index.toRoute.toMenu.findLoc(req).isDefined
    }

    "resolve to a template" withTemplateFor "/" in {
      _ == Templates("index" :: Nil)
    }
  }

  "Standard snails" should {

    "resolve to a template" withTemplateFor "/const" in {
      _ == Templates("const" :: Nil)
    }
  }
}
