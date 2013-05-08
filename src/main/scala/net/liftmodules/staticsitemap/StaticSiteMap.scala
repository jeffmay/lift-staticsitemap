package net.liftmodules.staticsitemap

import net.liftweb.sitemap.{Menu, Loc, SiteMap}

abstract class StaticSiteMap(val parent: Option[RoutesBuilder] = None)
  extends RoutesBuilder(Nil, Nil)
  with ConvertibleToSiteMap {

  def toSiteMap: SiteMap = SiteMap(menus: _*)

  def menus: List[Menu] = {
    routes.map(_.toMenu).toList
  }

  /**
   * Initialize the StaticSiteMap. Nothing needs to happen here.
   */
  def init() {}

  /**
   * Insert this LocParam into your menu if you want the
   * defined routes under the given menu.
   */
  case object AddStaticRoutesAfter extends Loc.AnyLocParam

  /**
   * replace the menu that has this LocParam with the static menu items
   */
  case object AddStaticRoutesHere extends Loc.AnyLocParam

  /**
   * Insert this LocParam into your menu if you want the
   * static menu items to be children of that menu
   */
  case object AddStaticRoutesUnder extends Loc.AnyLocParam


  private lazy val AfterUnapply = SiteMap.buildMenuMatcher(_ == AddStaticRoutesAfter)
  private lazy val HereUnapply = SiteMap.buildMenuMatcher(_ == AddStaticRoutesHere)
  private lazy val UnderUnapply = SiteMap.buildMenuMatcher(_ == AddStaticRoutesUnder)

  /**
   * The SiteMap mutator function
   */
  def sitemapMutator: SiteMap => SiteMap = SiteMap.sitemapMutator {
    case AfterUnapply(menu) => menu :: menus
    case HereUnapply(_) => menus
    case UnderUnapply(menu) => List(menu.rebuild(_ ::: menus))
  } (SiteMap.addMenusAtEndMutator(menus))
}
