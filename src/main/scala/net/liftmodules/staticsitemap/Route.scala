package net.liftmodules.staticsitemap

import net.liftweb.common.{Box, Full}
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftmodules.staticsitemap.path.PathParts

/**
 * The base trait of the Route and ParameterlessRoute classes.
 *
 * @tparam ParamsType The type of params to extract from the URL.
 *                    And often is required to build the URL.
 */
trait Route[ParamsType] extends ConvertableToMenu {

  def templatePath: PathParts

  def name: String

  def params: List[LocParam[ParamsType]]

  def >+(appendParam: LocParam[ParamsType]): Route[ParamsType]

  def >::(prependParam: LocParam[ParamsType]): Route[ParamsType]

  def >++(appendParams: List[LocParam[ParamsType]]): Route[ParamsType]

  def >:::(prependParams: List[LocParam[ParamsType]]): Route[ParamsType]
}

object Route {

  // TODO: Put this somewhere better.
  private[staticsitemap] def locPathFor(path: PathParts): List[LocPath] = (path match {
    case PathParts() => List("index")
    case path: PathParts => path.parts.toList.map(_.slug)
  }) map NormalLocPath

  /**
   * Helper method to construct a Route with a parameter.
   *
   * @param name A unique name for this route.
   * @param templatePath The path to the template that this Route is intended to render
   * @param linkText The text for the link in the generated SiteMap (probably won't use this, but
   *                 I've included it for flexibility)
   * @param paramForUrl A partial function that returns a boxed instance of a parameter given the
   *                    path of the current URL.
   * @param calcUrl A function that returns a URL given an instantiated parameter
   */
  def apply[ParamsType: Manifest](
    name: String,
    templatePath: PathParts,
    linkText: String,
    paramForUrl: PartialFunction[List[String], Box[ParamsType]],
    calcUrl: PartialFunction[Any, List[String]]): ParamsRoute[ParamsType] = {
    new ParamsRoute[ParamsType](name, templatePath, linkText, paramForUrl, calcUrl, Nil, Nil)
  }

  /**
   * Helper method to construct a Route without a parameter
   *
   * @param name A unique name for this route.
   * @param url The url that should match this route.
   * @param templatePath The path to the template that this Route is intended to render
   * @param linkText The text for the link in the generated SiteMap (probably won't use this, but
   *                 I've included it for flexibility)
   */
  def apply(
    name: String,
    url: String,
    templatePath: String,
    linkText: String): ParameterlessRoute = {
    new ParameterlessRoute(name, url, PathParts.fromAbsPath(templatePath), linkText, Nil)
  }
}