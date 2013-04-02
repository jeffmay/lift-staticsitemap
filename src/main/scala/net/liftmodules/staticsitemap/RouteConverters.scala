package net.liftmodules.staticsitemap

import net.liftweb.common.{Full, Box}
import net.liftweb.sitemap.Loc

trait ConvertibleToRoute[ParamsType] extends PathUtils {
  type RouteType = Route[ParamsType]
  protected implicit def paramsTypeManifest: Manifest[ParamsType]
  def templatePath: String
  val pathParts: List[PathPart] = PathPart.splitAbsPath(templatePath)
  /**
   * The name for the produced Menu Loc should be unique, but should prevent duplicate URL routes.
   */
  def name: String = mkFullPath(pathParts)
  def linkText: Loc.LinkText[ParamsType] = templatePath
  def params: List[Loc.LocParam[ParamsType]]
  protected def paramForUrl: PartialFunction[List[String], Box[ParamsType]]
  protected def urlForParam: PartialFunction[ParamsType, List[String]]
  // Override this if you want to update any parameter data after the URL is resolved and the parameter is extracted.
  protected def postExtractionHooks: Seq[Box[ParamsType] => Unit] = Nil
  def toRoute: RouteType
}
trait ConvertibleToRoute0 extends ConvertibleToRoute[Unit] {
  protected implicit val paramsTypeManifest: Manifest[Unit] = implicitly
  // Implement this as public to open this route for url lookup
  def url: String = templatePath
  override val pathParts: List[PathPart] = PathPart.splitAbsPath(url)
  protected def calcUrl = () => url
  protected def paramForUrl = {
    case `pathParts` => Full(())
  }
  protected def urlForParam = {
    case _ => PathPart.listPathPartsToListString(pathParts)
  }
  lazy val toRoute = ParameterlessRoute(name, url, templatePath, linkText, params)
}
trait ConvertibleToRoute1[T1] extends ConvertibleToRoute[T1] {
  def url(p1: T1): String
  protected def calcUrl = url(_)
  protected def urlForParam = {
    case param: T1 => calcUrl.andThen(PathUtils.splitPath(_))(param)
  }
  lazy val toRoute = ParamsRoute[T1](name, templatePath, linkText, paramForUrl, urlForParam, params, postExtractionHooks)
}
trait ConvertibleToRoute2[T1, T2] extends ConvertibleToRoute[(T1, T2)] {
  def url(p1: T1, p2: T2): String
  protected def calcUrl = Function.tupled(url _)
  protected def urlForParam = {
    case params: (T1, T2) => calcUrl.andThen(PathUtils.splitPath(_))(params)
  }
  lazy val toRoute = ParamsRoute[(T1, T2)](name, templatePath, linkText, paramForUrl, urlForParam, params, postExtractionHooks)
}
trait ConvertibleToRoute3[T1, T2, T3] extends ConvertibleToRoute[(T1, T2, T3)] {
  def url(p1: T1, p2: T2, p3: T3): String
  protected def calcUrl = Function.tupled(url _)
  protected def urlForParam = {
    case params: (T1, T2, T3) => calcUrl.andThen(PathUtils.splitPath(_))(params)
  }
  lazy val toRoute = ParamsRoute[(T1, T2, T3)](name, templatePath, linkText, paramForUrl, urlForParam, params, postExtractionHooks)
}

abstract class ParamsRouteConverter[ParamsType](
  override val templatePath: String,
  val params: List[Loc.LocParam[ParamsType]] = Nil
)(implicit val paramsTypeManifest: Manifest[ParamsType]) {
  this: ConvertibleToRoute[ParamsType] =>
}

abstract class ParameterlessRouteConverter(
  override val url: String,
  val templatePath: String,
  val params: List[Loc.LocParam[Unit]] = Nil
) extends ConvertibleToRoute0
{
  def this(templatePath: String, params: List[Loc.LocParam[Unit]] = Nil) = this(templatePath, templatePath, params)
}
