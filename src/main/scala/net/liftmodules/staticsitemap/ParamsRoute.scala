package net.liftmodules.staticsitemap

import net.liftweb.sitemap.{NormalLocPath, Loc}
import net.liftweb.common.{Empty, Box}
import net.liftweb.sitemap.Loc.LocParam
import net.liftweb.util.NamedPartialFunction
import net.liftweb.sitemap.Menu.ParamsMenuable
import net.liftmodules.staticsitemap.path.PathUtils._

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
  override val name: String,
  templatePath: String,
  override val linkText: Loc.LinkText[ParamsType],
  paramForUrl: PartialFunction[List[String], Box[ParamsType]],
  urlForParam: PartialFunction[ParamsType, List[String]],
  override val params: List[LocParam[ParamsType]],
  postExtractionHooks: Seq[NamedPartialFunction[Box[ParamsType], Unit]] = Nil
  )(implicit val mf: Manifest[ParamsType])
  extends ParamsMenuable[ParamsType](
    name,
    linkText,
    (path: List[String]) => {
      if (!paramForUrl.isDefinedAt(path)) Empty
      else paramForUrl(path)
    },
    (param: ParamsType) => {
      if (urlForParam.isDefinedAt(param))
        urlForParam(param)
      else throw new UrlGenerationException(name, param)
    },
    splitPath(templatePath).map(NormalLocPath(_)).toList,
    false,
    params,
    Nil
  )
  with Route[ParamsType] {
  route =>

  lazy val linkable =
    new ParamsLinkableLoc[ParamsType](
      name,
      templatePath,
      paramForUrl,
      urlForParam,
      linkText,
      params
    ) with PostExtractionHooks[ParamsType] {

      override def postExtraction(param: Box[ParamsType]) {
        postExtractionHooks foreach {
          f =>
            if (f.isDefinedAt(param)) {
              f.apply(param)
            }
        }
      }
    }

  override lazy val toLoc: Loc[ParamsType] = linkable

  override def >>(appendParam: LocParam[ParamsType]): ParamsMenuable[ParamsType] = this >+ appendParam

  def >+(appendParam: LocParam[ParamsType]): ParamsRoute[ParamsType] =
    ParamsRoute[ParamsType](
      name, templatePath, linkText, paramForUrl, urlForParam, params ::: List(appendParam),
      postExtractionHooks = postExtractionHooks
    )

  def >++(appendParams: List[LocParam[ParamsType]]): ParamsRoute[ParamsType] =
    ParamsRoute[ParamsType](
      name, templatePath, linkText, paramForUrl, urlForParam, params ::: appendParams,
      postExtractionHooks = postExtractionHooks
    )

  def >::(prependParam: LocParam[ParamsType]): ParamsRoute[ParamsType] =
    ParamsRoute[ParamsType](
      name, templatePath, linkText, paramForUrl, urlForParam, prependParam :: params,
      postExtractionHooks = postExtractionHooks
    )

  def >:::(prependParams: List[LocParam[ParamsType]]): Route[ParamsType] =
    ParamsRoute[ParamsType](
      name, templatePath, linkText, paramForUrl, urlForParam, prependParams ::: params,
      postExtractionHooks = postExtractionHooks
    )

  override def toString =
    "%s[%s[%s]](name=\"%s\", template=\"%s\", params=%s".format(
      getClass.getSimpleName,
      mf.erasure.getSimpleName,
      mf.erasure.getTypeParameters.map{_.getName}.mkString(","),
      name,
      templatePath,
      params
    ) + {
      if (postExtractionHooks.isEmpty) ")" else ", hooks=%s)".format(postExtractionHooks)
    }
}
