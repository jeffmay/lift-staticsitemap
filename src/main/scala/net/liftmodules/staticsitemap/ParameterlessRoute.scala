package net.liftmodules.staticsitemap

import net.liftweb.sitemap.{Menu, NormalLocPath, Loc}
import net.liftweb.sitemap.Loc.LocParam
import net.liftweb.util.NamedPartialFunction
import net.liftweb.common.Box
import net.liftweb.sitemap.Menu.Menuable
import net.liftmodules.staticsitemap.path.PathUtils._

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
  override val params: List[LocParam[Unit]],
  postExtractionHooks: Seq[NamedPartialFunction[Box[Unit], Unit]] = Nil)
  extends Menuable(
    name,
    linkText,
    splitPath(templatePath).map(NormalLocPath(_)).toList,
    false,
    params,
    Nil)
  with Route[Unit] {
  route =>

  lazy val linkable =
    new ParameterlessLinkableLoc(name, templatePath, url, linkText, params)
      with PostExtractionHooks[Unit] {

      override def postExtraction(param: Box[Unit]) {
        postExtractionHooks foreach {
          f =>
            if (f.isDefinedAt(param)) {
              f.apply(param)
            }
        }
      }
    }

  override lazy val toMenu: Menu = {
    Menu(linkable, submenus: _*)
  }

  override def >>(appendParam: Loc.LocParam[Unit]): Menuable = this >+ appendParam

  def >+(appendParam: Loc.LocParam[Unit]): ParameterlessRoute =
    ParameterlessRoute(name, url, templatePath, linkText, params ::: List(appendParam))

  def >++(appendParams: List[Loc.LocParam[Unit]]): ParameterlessRoute =
    ParameterlessRoute(name, url, templatePath, linkText, params ::: appendParams)

  def >::(prependParam: Loc.LocParam[Unit]): ParameterlessRoute =
    ParameterlessRoute(name, url, templatePath, linkText, prependParam :: params)

  def >:::(prependParams: List[Loc.LocParam[Unit]]): ParameterlessRoute =
    ParameterlessRoute(name, url, templatePath, linkText, prependParams ::: params)

  override def toString =
    "ParameterlessRoute(name=\"%s\", url=\"%s\", template=\"%s\", params=%s)"
      .format(url, name, templatePath, params)

}
