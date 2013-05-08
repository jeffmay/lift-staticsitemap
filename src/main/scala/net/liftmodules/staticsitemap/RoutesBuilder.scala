package net.liftmodules.staticsitemap

import net.liftweb.sitemap.{*, Loc}
import net.liftweb.sitemap.Loc.LocParam
import path._

abstract class RoutesBuilder(
  override val prefixNameParts: List[NormalPathPart],
  override val prefixParams: List[LocParam[Any]])
  extends RoutesContainer[Any] {
  container =>

  // Implicits for Url to TemplatePath mappings
  /**
   * Convert a String slug into a List[NormalPathPart] prepending the container's name prefix,
   * so that you can leave off the {{{^**}}}.
   */
  implicit def strToListPathParts(part: String): List[NormalPathPart] =
    prefixNameParts ::: List(NormalPathPart(part))

  /**
   * Convert a String slug -> absolute template path mapping into a List[NormalPathPart]
   * prepending the container's name prefix, so that you can leave off the {{{^**}}}.
   */
  implicit def tupleStrToTupleListPathPartStr(mapping: (String, String)): (List[NormalPathPart], String) =
    (prefixNameParts ::: List(NormalPathPart(mapping._1)), mapping._2)

  /**
   * Convert the url PathParts of a PathParts -> absolute template path mapping into a List[NormalPathPart]
   */
  implicit def tuplePathPartsStrToTupleListPathPartStr(mapping: (PathParts, String)): (List[NormalPathPart], String) =
    (mapping._1.parts.toList, mapping._2)

  /**
   * Perform any post construction initialization.
   */
  def init()

  /**
   * A route building helper that builds parameterless routes based on the given parameter types.
   *
   * The class / defines a container of routes generated by this object, so switching from a url
   * endpoint into a container of url endpoints is easy.
   *
   * {{{val Outer = ^** / "outer" / param1 / param2}}}
   *
   * becomes:
   *
   * {{{
   *   val Outer = new / ("outer", param1, param2) {
   *   val Inner = / ("inner")
   *     ...
   *   }
   * }}}
   */
  object @/ extends PathUtils {

    /**
     * Append multiple path parts to the end of the current prefix, separated by slashes.
     * @param parts The next parts of the url path
     * @param params Any loc params you want to append
     * @return a parameterless route to the template with the same filename as the path plus the ".html" suffix
     */
    def apply(parts: List[PathPart], params: LocParam[Any]*) = {
      // val pathParts = prefix ::: parts
      //      new ParameterlessSubRoute(pathParts, pathParts, (container.params ++ params).toList)
      new ParameterlessSubRoute(parts, parts, (container.prefixParams ++ params).toList)
    }

    /**
     * Append a list of parts to the end of the current prefix, separated by a slash.
     * @param mapping A mapping from the url, as constructed so far plus the given list of path parts,
     *                into (->) the path to the template
     * @param params Any loc params you want to append
     * @return a parameterless route to the template with the same filename as the path given in the mapping
     *         plus the ".html" suffix
     */
    def apply(mapping: (List[PathPart], String), params: LocParam[Any]*) = {
      //      val urlPathParts = prefix ::: mapping._1
      //      new ParameterlessSubRoute(urlPathParts, PathPart.splitAbsPath(mapping._2), (container.params ++ params).toList)
      new ParameterlessSubRoute(
        mapping._1,
        PathPart.splitAbsPath(mapping._2),
        (container.prefixParams ++ params).toList)
    }

    /**
     * Create a new routes container with an external RoutesBuilder or StaticSiteMap.
     * @param routes The RoutesBuilder or StaticSiteMap that you want to add to the site map
     * @param params Any loc params that you want to prefix to the contained Routes
     * @return A new / routes builder, added to the containing sitemap, with all the routes from the external routes builder.
     */
    def apply(routes: RoutesBuilder, params: Loc.AnyLocParam*) =
      new @/(routes, (container.prefixParams ++ params): _*) {}
  }

  /**
   * A routes container that allows you to prefix url parts
   * @param prefixNameParts parts of the path to prefix to the matched url
   * @param prefixParams loc params to prepend to all child routes
   */
  abstract class @/ private[this](prefixNameParts: List[PathPart], override val prefixParams: List[LocParam[Any]])
    extends RoutesBuilder(prefixNameParts, prefixParams) {

    def this(routes: RoutesBuilder, params: LocParam[Any]*) = {
      //      this(container.prefix ++ routes.prefix, (params ++ routes.params).toList)
      this(routes.prefixNameParts, (params ++ routes.prefixParams).toList)
      routes.routes foreach {addToRoutes(_)}
    }

    //    def this(prefix: List[PathPart], params: LocParam[Any]*) = this(container.prefix ::: prefix, params.toList)
    def this(prefixNameParts: List[PathPart], params: LocParam[Any]*) = this(prefixNameParts, params.toList)

    //    def this(params: LocParam[Any]*) = this(container.prefix, params.toList)
    def this(params: LocParam[Any]*) = this(Nil, params.toList)

    override val parent = Some(container)

    def init() {}
  }

  /**
   * Add an absolute url route without inheriting parent container's path prefix or loc params.
   *
   * TODO: Make this work with parent paths to form an absolute path.
   * TODO: Make this an inheritable class.
   */
  object :/ {

    /**
     * Append a part to the end of the current prefix, separated by a slash.
     * @param url The full url to match
     * @param params Any loc params you want to append
     * @return a parameterless route to the template with the same filename as the path plus the ".html" suffix
     */
    def apply(url: String, params: Loc.AnyLocParam*) = {
      val urlPathParts = PathPart.splitAbsPath(url)
      new ParameterlessSubRoute(urlPathParts, urlPathParts, params.toList)
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
      val urlPathParts = PathPart.splitAbsPath(mapping._1)
      new ParameterlessSubRoute(urlPathParts, PathPart.splitAbsPath(mapping._2), params.toList)
    }
  }

  /* Sub routes */

  /**
   * A RouteBuilder that maps a template to a url.
   * @param nameParts
   * @param templatePathParts
   * @param params
   */
  class ParameterlessSubRoute private[staticsitemap](
    nameParts: List[NormalPathPart],
    templatePathParts: List[NormalPathPart],
    params: List[LocParam[Unit]])
    extends ParameterlessRouteConverter(nameParts, templatePathParts, params) {

    def this(templatePath: String, params: LocParam[Unit]*) =
      this(PathPart.splitAbsPath(templatePath), PathPart.splitAbsPath(templatePath), params.toList)

    def this(mapping: (List[NormalPathPart], String), params: LocParam[Unit]*) =
      this(mapping._1, PathPart.splitAbsPath(mapping._2), params.toList)

    override val linkText = Loc.LinkText.strToLinkText(url)

    // Add routes to parent after all construction has completed
    addToRoutes(this.toRoute)
  }

  /**
   * An abstract RouteBuilder that adds the defined route to the sitemap.
   * @param templatePathParts The path to the template for this mapping
   * @tparam ParamsType The type of parameter or tuple of parameters
   */
  abstract class SubRoute[ParamsType : Manifest] private[staticsitemap](
    nameParts: List[NormalPathPart],
    templatePathParts: List[NormalPathPart],
    params: List[LocParam[ParamsType]])
    extends ParamsRouteConverter[ParamsType](nameParts, templatePathParts, params)
    with PathBuilder {

    // Since we don't know the url, we'll assume that the template path is the prefix
    // for url matching. Override this to get the / operator to match on a custom prefix.
    def this(templatePath: String, params: Loc.LocParam[ParamsType]*) =
      this(PathPart.splitAbsPath(templatePath), PathPart.splitAbsPath(templatePath), params.toList)

    def this(mapping: (List[NormalPathPart], String), params: Loc.LocParam[ParamsType]*) =
      this(mapping._1, PathPart.splitAbsPath(mapping._2), params.toList)

    // Override the path matcher to use the name parts
    override val parts = nameParts

    // Add routes to parent after all construction has completed
    addToRoutes(this.toRoute)
  }

  /*
    These are aliases to build common parameterized route types with prefix of its routes container.

    If you would like to place this inside of a prefixed routes container with a different prefix,
    then you must override the pathRoot val.

    If you don't want to add this to the sitemap at boot, then you must inherit from ParamsRouteConverter
   */
  protected abstract class String_@/ private[staticsitemap](
    nameParts: List[PathPart], templatePathParts: List[PathPart], params: Seq[Loc.LocParam[String]]
    ) extends Param_@/[String](nameParts, templatePathParts, params.toList) {

    def this(mapping: (List[PathPart], String), params: Loc.LocParam[String]*) =
      this(mapping._1, PathPart.splitAbsPath(mapping._2), params)

    def this(prefix: List[PathPart], params: Loc.LocParam[String]*) =
      this(prefix, prefix, params)

    def this(params: Loc.LocParam[String]*) = this(^** / *, params: _*)
  }

  protected abstract class Param_@/[T: Manifest] private[staticsitemap](
    nameParts: List[PathPart], templatePathParts: List[PathPart], params: Seq[Loc.LocParam[T]]
    ) extends SubRoute[T](nameParts, templatePathParts, params.toList) with ConvertibleToRoute1[T] {

    def this(mapping: (List[PathPart], String), params: Loc.LocParam[T]*) =
      this(mapping._1, PathPart.splitAbsPath(mapping._2), params.toList)

    def this(prefix: List[PathPart], params: Loc.LocParam[T]*) =
      this(prefix, prefix, params)

    def this(params: Loc.LocParam[T]*) = this(^** / *, params: _*)
  }

  protected abstract class TwoParam_@/[T1: Manifest, T2: Manifest] private[staticsitemap](
    nameParts: List[PathPart], templatePathParts: List[PathPart], params: Seq[Loc.LocParam[(T1, T2)]]
    ) extends SubRoute[(T1, T2)](nameParts, templatePathParts, params.toList) with ConvertibleToRoute2[T1, T2] {

    def this(mapping: (List[PathPart], String), params: Loc.LocParam[(T1, T2)]*) =
      this(mapping._1, PathPart.splitAbsPath(mapping._2), params)

    def this(prefix: List[PathPart], params: Loc.LocParam[(T1, T2)]*) =
      this(prefix, container.prefixNameParts ++ prefix, params)

    def this(params: Loc.LocParam[(T1, T2)]*) = this(^** / * / *, params: _*)
  }

  protected abstract class ThreeParam_@/[T1: Manifest, T2: Manifest, T3: Manifest] private[staticsitemap](
    nameParts: List[PathPart], templatePathParts: List[PathPart], params: Seq[Loc.LocParam[(T1, T2, T3)]]
    ) extends SubRoute[(T1, T2, T3)](nameParts, templatePathParts, params.toList) with ConvertibleToRoute3[T1, T2, T3] {

    def this(mapping: (List[PathPart], String), params: Loc.LocParam[(T1, T2, T3)]*) =
      this(mapping._1, PathPart.splitAbsPath(mapping._2), params)

    def this(prefix: List[PathPart], params: Loc.LocParam[(T1, T2, T3)]*) =
      this(prefix, prefix, params)

    def this(params: Loc.LocParam[(T1, T2, T3)]*) = this(^** / * / * / *, params: _*)
  }

}