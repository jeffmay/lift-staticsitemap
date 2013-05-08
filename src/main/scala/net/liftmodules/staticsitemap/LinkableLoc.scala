package net.liftmodules.staticsitemap

import net.liftweb.sitemap.Loc

/**
 * An internally used class that extends lift's Loc type to provide an additional url method that
 * can take arbitrary types as parameters and construct a URL from them.
 * @tparam ParamsType The type of the parameter for this route
 */
@deprecated("Inherit from Loc once CareMobile is using the StaticSiteMap", "161")
trait LinkableLoc[ParamsType] extends Loc[ParamsType] {

  @deprecated("Use StaticSiteMap routes", "161")
  def url(params: ParamsType): String

}
