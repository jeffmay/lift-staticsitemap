package net.liftmodules.staticsitemap

import net.liftweb.common.{Full, Box}
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Loc.LocParam
import net.liftmodules.staticsitemap.path.PathParts
import net.liftmodules.staticsitemap.path.PathUtils.splitPath

/**
 * A Loc that uses a partial function to match the URL and automatically rewrites it to the template path.
 * This is a simple implementation of the PFMatchingLoc trait for parameterized Locs.
 * @see [[net.liftmodules.staticsitemap.PFMatchingLoc]]
 *
 * @note this automatically assumes the MatchWithoutCurrentValue loc param, so including this is redundant.
 *
 * @param name The name of the Route that this Loc was generated from
 * @param params The LocParams of the Route that this Loc was generated from
 * @param text The link text of the Route that this Loc was generated from
 * @param templatePath The location of the template for this Loc
 * @param parameterForUrl The paramForUrl function of the Route that this Loc was generated from
 * @param urlForParameter The urlForParam function of the Route that this Loc was generated from
 * @tparam ParamsType The type of the parameter for this route
 */
class ParamsPFLoc[ParamsType](
  val name: String,
  val templatePath: PathParts,
  val parameterForUrl: PartialFunction[List[String], Box[ParamsType]],
  val urlForParameter: PartialFunction[ParamsType, List[String]],
  val text: Loc.LinkText[ParamsType],
  override val params: List[LocParam[ParamsType]]
) extends Loc[ParamsType]
  with PFMatchingLoc[ParamsType]


/**
 * A Loc that only matches a single URL string and automatically rewrites the path to the template.
 * This is a simple implementation of the PFMatchingLoc trait for parameterless / Unit Locs.
 * @see [[net.liftmodules.staticsitemap.PFMatchingLoc]]
 *
 * @param name The name of the Route that this Loc was generated from
 * @param templatePath The location of the template for this Loc
 * @param url The URL to match
 * @param text The link text of the Route that this Loc was generated from
 */
class StaticUrlPFLoc(
  val name: String,
  val templatePath: PathParts,
  val url: String,
  val text: Loc.LinkText[Unit],
  val params: List[LocParam[Unit]]
) extends PFMatchingLoc[Unit] {

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

/**
 * A builder for partial function matching locs that hides the fact that these have different types.
 */
object PFLoc {

  def apply[ParamsType](
    name: String,
    templatePath: PathParts,
    parameterForUrl: PartialFunction[List[String], Box[ParamsType]],
    urlForParameter: PartialFunction[ParamsType, List[String]],
    text: Loc.LinkText[ParamsType],
    params: List[LocParam[ParamsType]]
  ): PFMatchingLoc[ParamsType] = new ParamsPFLoc[ParamsType](name, templatePath, parameterForUrl, urlForParameter, text, params)

  def apply(
    name: String,
    templatePath: PathParts,
    url: String,
    text: Loc.LinkText[Unit],
    params: List[LocParam[Unit]]
  ): PFMatchingLoc[Unit] = new StaticUrlPFLoc(name, templatePath, url, text, params)

}