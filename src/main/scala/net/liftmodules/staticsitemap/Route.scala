package net.liftmodules.staticsitemap

import net.liftweb.common.{Box, Full}
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftmodules.staticsitemap.path.PathParts

/**
 * The base trait of Routes and ParameterlessRoutes
 *
 * @tparam ParamsType
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

  /**
   *
   * @param path
   * @return
   */
  def locPathFor(path: PathParts): List[LocPath] = (path match {
    case PathParts() => List("index")
    case path: PathParts => path.parts.toList map {_.slug}
  }) map {NormalLocPath(_)}

  /**
   * Helper method to find a route, with a parameter, with the given name.
   * @note This will only search entries in the sitemap that are instances of the LinkableLoc class
   *       (I.E. Routes constructed with the Route() apply methods that construct routes with parameters)
   *
   * @param name The name of the route to find
   */
  @deprecated("Use StaticSiteMap", "161")
  def find(name: String): LinkableLoc[Any] = {
    SiteMap.findLoc(name) match {
      case Full(loc: LinkableLoc[Any]) => loc
      case _ => throw new UndefinedRouteException(name, true)
    }
  }

  /**
   * Helper method to find a route, without a parameter, with the given name.
   * @note This will only search entries in the sitemap that are instances of the ParameterlessLinkableLoc class
   *       (I.E. Routes constructed with the Route() apply methods that construct routes without parameters)
   *
   * @param name The name of the route to find
   */
  @deprecated("Use StaticSiteMap", "161")
  def findParameterless(name: String): ParameterlessLinkableLoc = {
    SiteMap.findLoc(name) match {
      case Full(loc: ParameterlessLinkableLoc) => loc
      case _ => throw new UndefinedRouteException(name, false)
    }
  }

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