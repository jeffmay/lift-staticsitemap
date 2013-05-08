package net.liftmodules.staticsitemap

import net.liftweb.common.{Full, Box}
import net.liftweb.sitemap.{*, Loc}
import path.PathPart
import path.PathUtils._
import net.liftweb.util.NamedPartialFunction
import net.liftweb.http.S

trait ConvertibleToRoute[ParamsType] {

  type RouteType = Route[ParamsType]

  protected implicit def mf: Manifest[ParamsType]

  /** The absolute path to the template */
  def templatePath: String

  // By default use the template path as the URL path parts
  /** A list of path strings to match on */
  def urlPathParts: List[PathPart] = PathPart.splitAbsPath(templatePath)

  /** The parts to build the name from */
  def nameParts: List[PathPart] = urlPathParts

  /** The name for the produced Menu Loc should be unique, but should prevent duplicate URL routes. */
  def name: String = mkFullPath(nameParts)

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

  override def name: String = url

  override def nameParts: List[PathPart] = PathPart.splitAbsPath(url)

  def templatePath: String = url

  def paramForUrl = {
    case parts if (parts == urlPathParts) => Full(())
  }

  def urlForParam = {
    case _ => urlPathParts
  }

  lazy val toRoute = ParameterlessRoute(name, url, templatePath, linkText, locParams, postExtractionHooks)
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
    templatePath,
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
    templatePath,
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
    templatePath,
    linkText,
    paramForUrl,
    urlForParam,
    locParams,
    postExtractionHooks)
}

abstract class ParamsRouteConverter[ParamsType](
  override val nameParts: List[PathPart],
  templatePathParts: List[PathPart],
  override val locParams: List[Loc.LocParam[ParamsType]] = Nil
  )(implicit val mf: Manifest[ParamsType])
  extends ConvertibleToRoute[ParamsType] {

  override val urlPathParts: List[PathPart] = nameParts takeWhile { _ != PathPart(*) }
  override val templatePath: String = mkFullPath(templatePathParts)
}

abstract class ParameterlessRouteConverter(
  override val nameParts: List[PathPart],
  templatePathParts: List[PathPart],
  override val locParams: List[Loc.LocParam[Unit]] = Nil
  ) extends ConvertibleToRoute0 {

  override val urlPathParts: List[PathPart] = nameParts takeWhile { _ != PathPart(*) }
  override val templatePath: String = mkFullPath(templatePathParts)
  override val url: String = mkFullPath(urlPathParts)
}
