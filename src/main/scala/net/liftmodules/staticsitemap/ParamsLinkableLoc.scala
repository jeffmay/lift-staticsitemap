package net.liftmodules.staticsitemap

import net.liftweb.common.Box
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Loc.LocParam
import net.liftmodules.staticsitemap.path.PathParts

/**
 * @param name The name of the Route that this Loc was generated from
 * @param params The LocParams of the Route that this Loc was generated from
 * @param text The link text of the Route that this Loc was generated from
 * @param templatePath The location of the template for this Loc
 * @param parameterForUrl The paramForUrl function of the Route that this Loc was generated from
 * @param urlForParameter The urlForParam function of the Route that this Loc was generated from
 * @tparam ParamsType The type of the parameter for this route
 */
class ParamsLinkableLoc[ParamsType](
  val name: String,
  val templatePath: PathParts,
  val parameterForUrl: PartialFunction[List[String], Box[ParamsType]],
  val urlForParameter: PartialFunction[ParamsType, List[String]],
  val text: Loc.LinkText[ParamsType],
  override val params: List[LocParam[ParamsType]])
  extends LinkableLoc[ParamsType]
  with PartialFunctionMatching[ParamsType] {

  val matchHead_? = false

  @deprecated("Use StaticSiteMap")
  def url(param: ParamsType) = {
    if (urlForParameter.isDefinedAt(param))
      urlForParameter(param).mkString("/", "/", "")
    else throw new UrlGenerationException(name, param)
  }
}
