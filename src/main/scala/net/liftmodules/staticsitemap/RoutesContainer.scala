package net.liftmodules.staticsitemap

import net.liftweb.sitemap.ConvertableToMenu
import net.liftweb.sitemap.Loc.LocParam
import net.liftmodules.staticsitemap.path.{PathParts, NormalPathPart, PathBuilder}

trait RoutesContainer[LocParamsType] extends PathBuilder {
  // Some implicits for converting routeables and menuables
  implicit def convertToRoute[T](able: ConvertibleToRoute[T]): Route[T] = able.toRoute
  implicit def convertToMenu(able: ConvertableToMenu) = able.toMenu

  def parent: Option[RoutesContainer[_]]

  /**
   * Prefix all sub routes with this list of path parts.
   */
  def prefixNameParts: Seq[String]

  /**
   * Set the parts matcher to match the name parts by default.
   */
  override def parts: Seq[NormalPathPart] = prefixNameParts map {NormalPathPart(_)}

  /**
   * Prefix all sub routes with this sequence of loc params.
   */
  def prefixParams: List[LocParam[LocParamsType]]

  private[this] var _routes: List[Route[_]] = Nil

  /**
   * Retrieve the current, immutable sequence of routes.
   * @return The routes of this container
   */
  def routes: List[Route[_]] = _routes

  /**
   * Add the route to this container's routes
   * @param route The route to add.
   */
  def addToRoutes(route: Route[_]) {
    _routes ::= route
    parent foreach { _.addToRoutes(route) }
  }

  /**
   * Print the container class name, prefixed LocParams, and prefixed path parts
   */
  override def toString = "%s(%s)".format(
    getClass.getSimpleName,
    (
      ("" +: prefixNameParts.parts.map{_.slug}).mkString("^**", " / ", "") ::
        prefixParams.map{_.toString}
    ).mkString(", ")
  )

}
