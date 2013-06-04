package net.liftmodules.staticsitemap

import net.liftweb.common.{Full, Box}
import net.liftweb.sitemap.{*, Loc}
import net.liftmodules.staticsitemap.path.{PathParts, NormalPathPart, PathPart}
import path.PathUtils._
import net.liftweb.util.NamedPartialFunction

trait ConvertibleToRoute[ParamsType] {

  type RouteType = Route[ParamsType]

  protected implicit def mf: Manifest[ParamsType]

  /** The absolute path to the template */
  def templateParts: PathParts

  // By default use the template path as the URL path parts
  /** A list of path strings to match on */
  def urlPathParts: PathParts = templateParts

  /** The parts to build the name from */
  def nameParts: PathParts = urlPathParts

  /** The name for the produced Menu Loc should be unique, but should prevent duplicate URL routes. */
  def name: String = nameParts.asFullPath

  def linkText: Loc.LinkText[ParamsType] = name

  def locParams: List[Loc.LocParam[ParamsType]]

  def paramForUrl: PartialFunction[List[String], Box[ParamsType]]

  def urlForParam: PartialFunction[ParamsType, List[String]]

  // Override this if you want to update any parameter data after the URL is resolved and the parameter is extracted.
  def postExtractionHooks: Seq[NamedPartialFunction[Box[ParamsType], Unit]] = Nil

  def toRoute: RouteType
}

trait ConvertibleToRoute0 extends ConvertibleToRoute[Unit] {

  protected implicit def mf = manifest[Unit]

  // Implement this as public to open this route for url lookup
  def url: String

  override def nameParts: PathParts = PathParts.fromAbsPath(url)

  def templateParts: PathParts = PathParts.fromAbsPath(url)

  def paramForUrl = {
    case parts if (parts == urlPathParts) => Full(())
  }

  def urlForParam = {
    case _ => urlPathParts
  }

  lazy val toRoute = ParameterlessRoute(name, url, templateParts, linkText, locParams, postExtractionHooks)
}

trait ConvertibleToRoute1[T1] extends ConvertibleToRoute[T1] {

  def url(p1: T1): String

  private def calcUrl = url(_)

  /**
   * Optimistically calculate the url from the parameters.
   * No restriction on value or subtype.
   */
  def urlForParam = {
    case param: T1 => calcUrl.andThen(splitPath(_))(param)
  }

  lazy val toRoute = ParamsRoute[T1](
    name,
    templateParts,
    linkText,
    paramForUrl,
    urlForParam,
    locParams,
    postExtractionHooks)
}

trait ConvertibleToRoute2[T1, T2] extends ConvertibleToRoute[(T1, T2)] {

  def url(p1: T1, p2: T2): String

  private def calcUrl = Function.tupled(url _)

  /**
   * Optimistically calculate the url from the parameters.
   * No restriction on value or subtype.
   */
  def urlForParam = {
    case params: (T1, T2) => calcUrl.andThen(splitPath(_))(params)
  }

  lazy val toRoute = ParamsRoute[(T1, T2)](
    name,
    templateParts,
    linkText,
    paramForUrl,
    urlForParam,
    locParams,
    postExtractionHooks)
}

trait ConvertibleToRoute3[T1, T2, T3] extends ConvertibleToRoute[(T1, T2, T3)] {

  def url(p1: T1, p2: T2, p3: T3): String

  private def calcUrl = Function.tupled(url _)

  /**
   * Optimistically calculate the url from the parameters.
   * No restriction on value or subtype.
   */
  def urlForParam = {
    case params: (T1, T2, T3) => calcUrl.andThen(splitPath(_))(params)
  }

  lazy val toRoute = ParamsRoute[(T1, T2, T3)](
    name,
    templateParts,
    linkText,
    paramForUrl,
    urlForParam,
    locParams,
    postExtractionHooks)
}

abstract class ParamsRouteConverter[ParamsType](
  override val nameParts: PathParts,
  override val templateParts: PathParts,
  override val locParams: List[Loc.LocParam[ParamsType]] = Nil
  )(implicit val mf: Manifest[ParamsType])
  extends ConvertibleToRoute[ParamsType] {

  override val urlPathParts: PathParts = PathParts(nameParts.parts.takeWhile{_ != PathPart(*)}: _*)
}

abstract class ParameterlessRouteConverter(
  override val nameParts: PathParts,
  override val templateParts: PathParts,
  override val locParams: List[Loc.LocParam[Unit]] = Nil
  ) extends ConvertibleToRoute0 {

  override val urlPathParts: PathParts = PathParts(nameParts.parts.takeWhile{_ != PathPart(*)}: _*)
  override val url: String = urlPathParts.asFullPath
}
