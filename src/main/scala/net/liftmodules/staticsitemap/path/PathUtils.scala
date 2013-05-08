package net.liftmodules.staticsitemap.path

import net.liftweb.common.{Full, Empty}
import net.liftweb.util.Helpers._

trait PathUtils {

  /**
   * Split the path and query.
   * @param url
   * @return A tuple of optional path and query strings
   */
  def splitPathAndQuery(url: String): (String, String) =
    url.split("\\?", 2).toList match {
      case Nil => ("", "")
      case path :: Nil => (path, "")
      case path :: queryString :: Nil => (path, queryString)
      case _ => throw new IllegalStateException("Url was split into more than two parts for the path and query string.")
    }

  def takePath(url: String): String = splitPathAndQuery(url)._1

  def takeQuery(url: String): String = splitPathAndQuery(url)._2

  def splitPath(url: String): List[String] = {
    takePath(url) match {
      case "" => Nil
      case path => path.split('/').filterNot(_.isEmpty).toList
    }
  }

  /**
   * Extract all parameters from the URL.
   *
   * Cribbed from Lift's Req.scala
   *
   * @param url The full url to parse
   * @return a map of parameter name to list of values for that parameter
   */
  def splitParams(url: String): Map[String, List[String]] = {
    val queryString = takeQuery(url)
    val params: List[(String, String)] = for {
      nameVal <- queryString.split("&").toList.map(_.trim).filter(_.length > 0)
      (name, value) <- nameVal.split("=").toList match {
        case Nil => Empty
        case n :: v :: _ => Full((urlDecode(n), urlDecode(v)))
        case n :: _ => Full((urlDecode(n), ""))
      }} yield (name, value)

    val nvp: Map[String, List[String]] = params.foldLeft(Map[String, List[String]]()) {
      case (map, (name, value)) => map + (name -> (map.getOrElse(name, Nil) ::: List(value)))
    }
    nvp
  }

  def mkFullPath(urlParts: Seq[String]): String =
    urlParts.mkString("/", "/", "")
}
object PathUtils extends PathUtils