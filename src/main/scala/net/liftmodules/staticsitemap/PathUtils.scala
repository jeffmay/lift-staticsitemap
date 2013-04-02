package net.liftmodules.staticsitemap

trait PathUtils {
  def splitPath(url: String): List[String] = {
    url.split('/').filterNot(_.isEmpty).toList
  }

  def mkFullPath(urlParts: Seq[String]): String = urlParts.mkString("/", "/", "")
}

object PathUtils extends PathUtils
