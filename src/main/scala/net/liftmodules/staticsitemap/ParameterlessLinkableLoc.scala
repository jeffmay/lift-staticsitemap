package net.liftmodules.staticsitemap

import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Loc.LocParam
import net.liftweb.common.Full
import path.PathUtils._

/**
 * An internally used class that extends lift's Loc type to provide an additional url attribute.
 * @param name The name of the Route that this Loc was generated from
 * @param url The url that this Loc represents
 * @param params The LocParams of the Route that this Loc was generated from
 * @param text The link text of the Route that this Loc was generated from
 */
class ParameterlessLinkableLoc(
  override val name: String,
  val templatePath: String,
  val url: String,
  override val text: Loc.LinkText[Unit],
  override val params: List[LocParam[Unit]])
  extends LinkableLoc[Unit] with PartialFunctionMatching[Unit] {

  @deprecated("Use StaticSiteMap routes", "161")
  def url(params: Unit) = url

  val matchHead_? = false

  override val defaultValue = Full(())

  private val urlParts = splitPath(url)

  def urlForParameter = {
    case _ => urlParts
  }

  def parameterForUrl = {
    case `urlParts` =>
      defaultValue
  }
}
