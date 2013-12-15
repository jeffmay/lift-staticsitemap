package net.liftmodules.staticsitemap

import net.liftweb.mockweb.WebSpec
import net.liftweb.common.{Empty, Full, Box, Failure}
import scala.xml.NodeSeq
import net.liftweb.http.{S, ParsePath, LiftRules, Templates}
import org.specs2.execute.{Result, Success, Error}

class SampleSiteMap extends StaticSiteMap {
  val Index = @/(^)
  val Const = @/(^ / "const")
}

object SampleSiteMapBoot {

  val sitemap = new SampleSiteMap

  def boot() {
    LiftRules.setSiteMap(sitemap.toSiteMap)
  }
}

class RootIndexTest extends WebSpec(SampleSiteMapBoot.boot) {

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

  import SampleSiteMapBoot.sitemap._

  /*
    All templates are located in the test webapp directory.
   */

  "The SiteMap" should {
    "not resolve a non existant url" withReqFor "/empty" in {
      _.location.isEmpty
    }
  }

  "Root Index" should {

    "resolve the / url" withReqFor "/" in {
      _.location.isDefined
    }

    "resolve to the static Index loc" withReqFor "/" in {
      req => Index.toRoute.toMenu.findLoc(req).isDefined
    }

    "resolve to a template" withTemplateFor "/index" in {
      case Full(html) =>
        val found = Templates("index" :: Nil).openOrThrowException("expected test")
        found.theSeq === html.theSeq
        pending("MockWeb doesn't seem to find the template for the root path, but it works live.")
      case Failure(_, Full(ex), _) => Error(ex)
      case Failure(msg, _, _) => Error(msg)
      case Empty => Error("Could not find template for path \"/index\"")
    }
  }

  "Standard snails" should {

    "resolve to a template" withTemplateFor "/const" in {
      case Full(html) =>
        val found = Templates("const" :: Nil).openOrThrowException("expected test")
        found.theSeq === html.theSeq
      case Failure(_, Full(ex), _) => Error(ex)
      case Failure(msg, _, _) => Error(msg)
      case Empty => Error("Could not find template for path \"/const\"")
    }
  }
}
