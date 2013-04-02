package net.liftmodules.staticsitemap

import net.liftweb.common.{Empty, Box}
import net.liftweb.sitemap.Menu.{Menuable, ParamsMenuable}
import net.liftweb.sitemap._
import net.liftweb.http._
import net.liftweb.sitemap.Loc._
import net.liftweb.util.NamedPF
import net.liftweb.util.Helpers._
import net.liftweb.common.Full
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import PathUtils.splitPath

/**
 * Hack around Lift's ugly parameter extraction code.
 * Instead of matching the route with some weird recursive string matching method
 * it uses a partial function to determine if a route matches the current URL.
 * @tparam ParameterT The type of the parameter for this route
 */
trait ParameterExtractor[ParameterT] {
  this: Loc[ParameterT] =>

  def parameterForUrl: PartialFunction[List[String], Box[ParameterT]]

  def templatePath: String

  override val defaultValue: Box[ParameterT] = Empty

  object unathenticatedRewrite extends RequestVar[Boolean](false) {
    override val __nameSalt = randomString(10)
  }

  /**
   * Override the default lift matching logic with a hack so the
   * request will be considered up until its TestAccess LocParam is run.
   * This way we don't run the body of paramForUrl if the current user
   * is unauthenticated, but allow for redirects to other pages instead
   * of throwing a 404.
   */
  override def doesMatch_?(req: Req): Boolean = {
    (if (link.isDefinedAt(req)) {
      link(req) match {
        case Full(x) if
          allParams.forall {
            case Loc.Test(test) => test(req)
            case _ => true
          } => x
        case Full(x) => false
        case x => x.openOr(false)
      }
    } else false) && (currentValue.isDefined || unathenticatedRewrite.get)
  }

  override lazy val rewritePF: Box[LiftRules.RewritePF] =
    Full(NamedPF(templatePath) {
      new PartialFunction[RewriteRequest, RewriteResponse] {

        def isDefinedAt(req: RewriteRequest) = {
          req.path match {
            case ParsePath(list, "", true, endingSlash) =>
              val parts = if (endingSlash) list.init else list
              parameterForUrl.isDefinedAt(parts)
            case _ => false
          }
        }

        def apply(req: RewriteRequest): RewriteResponse = {
          val param =
            req.path match {
              case ParsePath(list, "", true, endingSlash) =>
                val parts = if (endingSlash) list.init else list
                if(S.loggedIn_?) parameterForUrl(parts)
                else {
                  unathenticatedRewrite.set(true)
                  Empty
                }
              case _ => Empty
            }
          requestValue.set(param)
          postExtraction(param)
          RewriteResponse(splitPath(templatePath), true)
        }
      }
    }
  )

  /**
   * A method that is called after the parameter is extracted from the URL.
   * @param param
   */
  def postExtraction(param: Box[ParameterT])
}

/**
 * An internally used class that extends lift's Loc type to provide an additional url method that
 * can take arbitrary types as parameters and construct a URL from them.
 * @tparam ParamType The type of the parameter for this route
 */

trait LinkableLoc[ParamType] extends Loc[ParamType] {

  // TODO: Use currentValue or calcDefaultHref and remove this?
  def url(params: ParamType): String

}

/**
 * @param name The name of the Route that this Loc was generated from
 * @param params The LocParams of the Route that this Loc was generated from
 * @param linkText The link text of the Route that this Loc was generated from
 * @param templatePath The location of the template for this Loc
 * @param parameterForUrl The paramForUrl function of the Route that this Loc was generated from
 * @param urlForParam The urlForParam function of the Route that this Loc was generated from
 * @tparam ParameterT The type of the parameter for this route
 */
abstract class LinkableLocExtractor[ParameterT](
  override val name: String,
  override val templatePath: String,
  override val parameterForUrl: PartialFunction[List[String], Box[ParameterT]],
  urlForParam: PartialFunction[ParameterT, List[String]],
  linkText: Loc.LinkText[ParameterT],
  override val params: List[LocParam[ParameterT]]
) extends LinkableLoc[ParameterT] with ParameterExtractor[ParameterT] {

  def url(param: ParameterT) = {
    if(urlForParam.isDefinedAt(param))
      urlForParam(param).mkString("/", "/", "")
    else throw new UrlGenerationException(name, param)
  }

  lazy val text = linkText
}

/**
 * An internally used class that extends lift's Loc type to provide an additional url attribute.
 * @param name The name of the Route that this Loc was generated from
 * @param url The url that this Loc represents
 * @param params The LocParams of the Route that this Loc was generated from
 * @param linkText The link text of the Route that this Loc was generated from
 */
abstract class ParameterlessLinkableLoc(
  override val name: String,
  templatePath: String,
  val url: String,
  linkText: Loc.LinkText[Unit],
  override val params: List[LocParam[Unit]]
) extends LinkableLoc[Unit] {

  def url(ignore: Unit = ()) = url

  override val defaultValue = Full(())

  lazy val text = linkText
}

/**
 * Exception thrown when a URL cannot be constructed with the parameter passed to the url() method of a Loc.
 * @param routeName
 * @param param
 */
class UrlGenerationException(
  routeName: String,
  param: Any
) extends Exception {
  override def getMessage(): String = {
    """The Route %s cannot generate a link for the parameter %s.
      |Please either:
      |1. Update this route's paramForUrl partial function to be defined
      |for parameters with this value.
      |2. Update the call to url() on the Route's Loc that threw this exception
      |to pass a valid parameter.
    """.stripMargin.format(routeName, param.toString)
  }
}

/**
 * The base trait of Routes and ParameterlessRoutes
 *
 * @tparam ParamsType
 */
trait Route[ParamsType] extends ConvertableToMenu {
  def templatePath: String
  def name: String
  def >-(param: Loc.LocParam[ParamsType]): Route[ParamsType]
  def >-:(param: Loc.LocParam[ParamsType]): Route[ParamsType]
}

/**
 * A Route parameterized by the type of its parameter. This is a helper that should make
 * writing RESTful URLs and snippets significantly easier.
 *
 * @param name A unique name for this route.
 * @param templatePath The path to the template that this Route is intended to render
 * @param linkText The text for the link in the generated SiteMap (probably won't use this, but
 *                 I've included it for flexibility)
 * @param paramForUrl A partial function that returns a boxed instance of a parameter given the
 *                    path of the current URL.
 * @param urlForParam A function that returns a URL given an instantiated parameter
 * @param params A list of LocParams (this is used for compatibility with Lift's own Locs, but you'd probably
 *               rather add them by using the >> method).
 * @tparam ParamsType The type of the parameter for this route
 */
case class ParamsRoute[ParamsType](
  override val name : String,
  templatePath: String,
  override val linkText: Loc.LinkText[ParamsType],
  paramForUrl: PartialFunction[List[String], Box[ParamsType]],
  urlForParam: PartialFunction[ParamsType, List[String]],
  override val params: List[LocParam[ParamsType]],
  postExtractionHooks: Seq[Box[ParamsType] => Unit] = Nil
)(implicit val mf: Manifest[ParamsType])
  extends ParamsMenuable[ParamsType](
    name,
    linkText,
    (path: List[String]) => {
      if (!paramForUrl.isDefinedAt(path)) Empty
      else paramForUrl(path)
    },
    (param: ParamsType) => {
      if(urlForParam.isDefinedAt(param))
        urlForParam(param)
      else throw new UrlGenerationException(name, param)
    },
    splitPath(templatePath).map(NormalLocPath(_)).toList,
    false,
    params,
    Nil
  )
  with Route[ParamsType]
{ route =>

  lazy val linkable =
    new LinkableLocExtractor[ParamsType](
      name,
      templatePath,
      paramForUrl,
      urlForParam,
      linkText,
      params
    ) {
      val link = new Link[ParamsType](List(name), route.headMatch) {
        override def isDefinedAt(req: Req): Boolean =
          req.path.partPath == splitPath(templatePath)
        override def pathList(value: ParamsType): List[String] =
          route.encoder(value)
      }

      def postExtraction(param: Box[ParamsType]) {
        postExtractionHooks map { _.apply(param) }
      }
    }

  override lazy val toLoc: Loc[ParamsType] = linkable

  override def >>(param: Loc.LocParam[ParamsType]): ParamsMenuable[ParamsType] = this >- param

  def >-(param: Loc.LocParam[ParamsType]): ParamsRoute[ParamsType] =
    ParamsRoute[ParamsType](name, templatePath, linkText, paramForUrl, urlForParam, params ::: List(param),
      postExtractionHooks = postExtractionHooks
    )

  def >-:(param: Loc.LocParam[ParamsType]): ParamsRoute[ParamsType] =
    ParamsRoute[ParamsType](name, templatePath, linkText, paramForUrl, urlForParam, param :: params,
      postExtractionHooks = postExtractionHooks
    )

  override def toString =
    "Route[%s](name=\"%s\", template=\"%s\", params=\"%s\")".format(mf.erasure, name, templatePath, params)
}

/**
 * A Route without a parameter.
 *
 * @param name A unique name for this route.
 * @param url The url that should match this route.
 * @param templatePath The path to the template that this Route is intended to render
 * @param linkText The text for the link in the generated SiteMap (probably won't use this, but
 *                 I've included it for flexibility)
 */
case class ParameterlessRoute(
  override val name: String,
  url: String,
  templatePath: String,
  override val linkText: Loc.LinkText[Unit],
  override val params: List[LocParam[Unit]]
)
  extends Menuable(
    name,
    linkText,
    splitPath(templatePath).map(NormalLocPath(_)).toList,
    false,
    params,
    Nil
  )
  with Route[Unit]
{ route =>

  protected def isDefinedAt(path: ParsePath): Boolean = {
    path match {
      case ParsePath(list, "", true, endingSlash) =>
        val parts = if (endingSlash) list.init else list
        splitPath(url) == parts
      case _ => false
    }
  }

  lazy val linkable =
    new ParameterlessLinkableLoc(name, templatePath, url, linkText, params) {
      override def rewrite: LocRewrite = Full(
        new PartialFunction[RewriteRequest, (RewriteResponse, Unit)] {
          def isDefinedAt(req: RewriteRequest): Boolean = {
            route.isDefinedAt(req.path)
          }

          /**
           * Rewrite the URL to match the template path to ensure a unique identifier of this route.
           */
          def apply(req: RewriteRequest): (RewriteResponse, Unit) = {
            (RewriteResponse(splitPath(route.templatePath), true), ())
          }
        }
      )

      val link = new Link[Unit](List(name), route.headMatch) {
        /**
         * Match the request part path to the template path of this route.
         */
        override def isDefinedAt(req: Req): Boolean = {
          splitPath(route.templatePath) == req.path.partPath
        }

        override def pathList(value: Unit): List[String] = splitPath(route.templatePath)
      }
    }

  override lazy val toMenu: Menu = {
    Menu(linkable, submenus: _*)
  }

  override def >>(param: Loc.LocParam[Unit]): Menuable =
    ParameterlessRoute(name, url, templatePath, linkText, params ::: List(param))

  override def >-(param: Loc.LocParam[Unit]): Route[Unit] =
    ParameterlessRoute(name, url, templatePath, linkText, params ::: List(param))

  override def >-:(param: Loc.LocParam[Unit]): Route[Unit] =
    ParameterlessRoute(name, url, templatePath, linkText, param :: params)

  override def toString =
    "ParameterlessRoute(name=\"%s\", url=\"%s\", template=\"%s\", params=\"%s\")"
      .format(url, name, templatePath, params)

}

/**
 * Exception thrown when a route cannot be found with the specified name.
 * @param routeName The name of the route that was searched for
 * @param withParameter Whether or not the route was expected to have a parameter
 */
class UndefinedRouteException(
  routeName: String,
  withParameter: Boolean
) extends Exception {
  override def getMessage(): String = {
    if(withParameter)
      """Route "%s" does not exist in the sitemap.""".format(routeName)
    else
      """Parameterless route "%s" does not exist in the sitemap.""".format(routeName)
  }
}


object Route {

  /**
   * Helper method to find a route, with a parameter, with the given name.
   * @note This will only search entries in the sitemap that are instances of the LinkableLoc class
   *       (I.E. Routes constructed with the Route() apply methods that construct routes with parameters)
   *
   * @param name The name of the route to find
   */
  def find(name: String): LinkableLoc[Any] = {
    SiteMap.findLoc(name) match {
      case Full(loc: LinkableLoc[Any]) => loc
      case _ => throw new UndefinedRouteException(name, true)
    }
  }

  /**
   * Helper method to find a route, without a parameter, with the given name.
   * @note This will only search entries in the sitemap that are instances of the ParameterlessLinkableLoc class
   *       (I.E. Routes constructed with the Route() apply methods that construct routes without parameters)
   *
   * @param name The name of the route to find
   */
  def findParameterless(name: String): ParameterlessLinkableLoc = {
    SiteMap.findLoc(name) match {
      case Full(loc: ParameterlessLinkableLoc) => loc
      case _ => throw new UndefinedRouteException(name, false)
    }
  }

  /**
   * Helper method to construct a Route with a parameter.
   *
   * @param name A unique name for this route.
   * @param templatePath The path to the template that this Route is intended to render
   * @param linkText The text for the link in the generated SiteMap (probably won't use this, but
   *                 I've included it for flexibility)
   * @param paramForUrl A partial function that returns a boxed instance of a parameter given the
   *                    path of the current URL.
   * @param calcUrl A function that returns a URL given an instantiated parameter
   */
  def apply[ParameterT : Manifest](
    name: String,
    templatePath: String,
    linkText: String,
    paramForUrl: PartialFunction[List[String], Box[ParameterT]],
    calcUrl: PartialFunction[Any, List[String]]
  ): ParamsRoute[ParameterT] = {
    new ParamsRoute[ParameterT](name, templatePath, linkText, paramForUrl, calcUrl, Nil, Nil)
  }

  /**
   * Helper method to construct a Route without a parameter
   *
   * @param name A unique name for this route.
   * @param url The url that should match this route.
   * @param templatePath The path to the template that this Route is intended to render
   * @param linkText The text for the link in the generated SiteMap (probably won't use this, but
   *                 I've included it for flexibility)
   */
  def apply(
    name: String,
    url: String,
    templatePath: String,
    linkText: String
  ): ParameterlessRoute =
  {
    new ParameterlessRoute(name, url, templatePath, linkText, Nil)
  }
}