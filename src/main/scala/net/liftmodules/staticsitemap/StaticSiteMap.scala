package net.liftmodules.staticsitemap

import net.liftweb.sitemap.{Menu, Loc, SiteMap}

trait StaticSiteMap extends RoutesBuilder {

  /**
   * Add an absolute url route without inheriting parent container's path prefix or loc params.
   */
  object :/ {
    /**
     * Append a part to the end of the current prefix, separated by a slash.
     * @param url The full url to match
     * @param params Any loc params you want to append
     * @return a parameterless route to the template with the same filename as the path plus the ".html" suffix
     */
    def apply(url: String, params: Loc.AnyLocParam*) = {
      / (PathPart.splitAbsPath(url), params: _*)
    }

    /**
     * Append a part to the end of the current prefix, separated by a slash, with a specified template path.
     * @param mapping A mapping from the url, as constructed so far plus the given path part,
     *                into (->) the path to the template
     * @param params Any loc params you want to append
     * @return a parameterless route from the url in the first element of the mapping to the template with the
     *         filename name given in the second element of the mapping.
     */
    def apply(mapping: (String, String), params: Loc.AnyLocParam*) = {
      / (PathPart.splitAbsPath(mapping._1) -> mapping._2, params: _*)
    }
  }

  def toSiteMap: SiteMap = SiteMap(menus: _*)

  def menus: List[Menu] = {
    routes.map(_.toMenu).toList
  }

  /**
   * Initialize the StaticSiteMap. Nothing needs to happen here.
   */
  def init {}

  /**
   * Insert this LocParam into your menu if you want the
   * defined routes under the given menu.
   */
  final case object AddStaticRoutesAfter extends Loc.AnyLocParam

  /**
   * replace the menu that has this LocParam with the static menu items
   */
  final case object AddStaticRoutesHere extends Loc.AnyLocParam

  /**
   * Insert this LocParam into your menu if you want the
   * static menu items to be children of that menu
   */
  final case object AddStaticRoutesUnder extends Loc.AnyLocParam


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
