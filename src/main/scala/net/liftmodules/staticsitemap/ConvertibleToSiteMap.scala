package net.liftmodules.staticsitemap

import net.liftweb.sitemap.SiteMap

trait ConvertibleToSiteMap {
  def toSiteMap: SiteMap
}
