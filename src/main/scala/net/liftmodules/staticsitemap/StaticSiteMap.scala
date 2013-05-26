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
   * Attempt to cast the container to a specific static container of routes in an Option.
   *
   * @tparam T The type of RoutesContainer to convert to.
   * @return An option of type T
   */
  def getAs[T](implicit mf: Manifest[T]): Option[T] = try {
    Some(mf.runtimeClass.cast(this).asInstanceOf[T])
  }
  catch {
    case _: ClassCastException => None
  }

  /**
   * Attempt to cast the container to a specific static container of routes or else convert to some
   * subtype of T.
   *
   * @tparam T The type of RoutesContainer to convert to.
   * @return A RoutesContainer of type T
   */
  def getAsOrElse[T : Manifest](otherwise: => T): T = this.getAs[T].getOrElse(otherwise)

  /**
   * Attempt to cast the container to a specific static container of routes.
   *
   * This is useful for doing:
   * sitemap.getAsOrThrow[SpecialRoutes].Special.url
   *
   * @tparam T The type of RoutesContainer to convert to.
   * @return A RoutesContainer of type T
   * @throws MissingRoutesException When it can't be cast
   */
  def getAsOrThrow[T : Manifest]: T = this.getAsOrElse[T](throw new MissingRoutesException[this.type, T])

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
