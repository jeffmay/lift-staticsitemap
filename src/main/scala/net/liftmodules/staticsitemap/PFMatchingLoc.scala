package net.liftmodules.staticsitemap

import net.liftweb.common._
import net.liftweb.util.NamedPF
import net.liftweb.http._
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Loc.{EarlyResponse, Link}
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import net.liftmodules.staticsitemap.path.PathParts

/**
 * Uses a partial function's isDefinedAt method to determine if this Loc matches.
 *
 * The matching logic happens in two phases.
 *
 * 1. The matching phases:
 *
 * In this phase, Lift is searching through all known Locs for the one who's doesMatch_?
 * method returns true. Once there is one who does match, the search stops and the request's
 * loc value is set.
 *
 * At this point, the logic differs from a normal Loc in that it will store the name of the loc
 * that matched in a RequestVar and then automatically issue a rewrite to the template path
 * with the matched value -- rather than the same URL with the matched value.
 *
 * 2. Post-rewrite matching phase:
 *
 * In this phase we cannot trust our partial function to match anymore, since the template
 * path may differ from the URL path. So in this phase we check whether the URL matches
 * by checking whether our name is the same as the name of the matched loc in the RequestVar.
 *
 * This second phase may still match another non-PFMatchingLoc but it will never match a
 * different PFMatchingLoc. Depending on how you are using it, this may be a feature or a bug,
 * so be careful with mixing PFMatchingLocs and normal Locs.
 *
 * @tparam ParamsType The type of the parameter for this route.
 */
trait PFMatchingLoc[ParamsType] extends Loc[ParamsType] with LazyLoggable {
  loc =>

  def parameterForUrl: PartialFunction[List[String], Box[ParamsType]]

  def urlForParameter: PartialFunction[ParamsType, List[String]]

  def templatePath: PathParts

  def defaultValue: Box[ParamsType] = Empty


  /*
   * Hooks into the partial function matching logic
   */

  /**
   * Called after the loc first matches the request.
   */
  def onMatch() {}

  /**
   * Calls your post extraction hook after the value is extracted
   * from the partial function, and before the
   */
  def postExtraction(param: Box[ParamsType]) {}


  /**
   * A request value that holds on to any redirect exception received while matching a request.
   *
   * If the parameterForUrl method attempts to redirect while matching a request,
   * we intercept it, this Loc will be attached to the request, and the rewrite rule will
   * rewrite to this url instead of the template path.
   */
  private object capturedEarlyResponse extends RequestVar[Box[() => Box[LiftResponse]]](Empty) {

    override val __nameSalt = randomString(10)
  }

  /**
   * Safely check whether the parameterForUrl method can parse the given path parts.
   * @param parts path parts of a request.
   * @return true if the parameter partial function is defined or a redirect exception is thrown,
   *         false if the parameter partial function is not defined or any other type of exception is thrown.
   */
  def canHandle(parts: List[String]): Boolean = {
    try parameterForUrl isDefinedAt parts
    catch {
      case captured: ResponseShortcutException => true
      case _: Exception => false
    }
  }

  /**
   * Safely check whether the parameterForUrl method can parse the given path parts.
   * @param parsePath a request parse path.
   * @return true if the parameter partial function is defined or a redirect exception is thrown,
   *         false if the parameter partial function is not defined, the path cannot be extracted,
   *         or any other type of exception is thrown.
   */
  def canHandle(parsePath: ParsePath): Boolean = {
    extractParts(parsePath) match {
      case Some(parts) => this canHandle parts
      case None => false
    }
  }

  /**
   * Safely try to parse the value.
   *
   * This will properly handle any redirects thrown by the parameterForUrl
   * method and wrap them in a failure.
   *
   * Since we are still in the process of matching a Loc, we cannot let the
   * exception bubble up until we are safely within the session phase of the
   * request lifecycle.
   *
   * @param parts path parts of a url.
   * @return A box containing the parameter for the parse path.
   */
  def safeExtractValue(parts: List[String]): Box[ParamsType] = {
    if (this canHandle parts)
      try parameterForUrl apply parts
      catch {
        case captured: ResponseShortcutException =>
          Failure("ResponseShortcutException", Full(captured), Empty)
      }
    else
      Empty
  }

  /**
   * Safely try to parse the value.
   * @param parsePath a request parse path.
   * @return A box containing the parameter for the parse path.
   */
  def safeExtractValue(parsePath: ParsePath): Box[ParamsType] = {
    extractParts(parsePath) match {
      case Some(parts) => safeExtractValue(parts)
      case None => Empty
    }
  }

  /**
   * Parse a request's ParsePath to retrieve a Box containing the parameter type
   * or an early response shortcut.
   */
  def calcValueOrResponse(path: ParsePath): Either[Box[ParamsType], () => Box[LiftResponse]] = {
    safeExtractValue(path) match {
      case Failure(_, Full(captured: ResponseShortcutException), _) =>
        Right(() => Full(captured.response))
      case x =>
        Left(x)
    }
  }

  /**
   * Extract the parts from a parse path.
   * @param path the ParsePath of the request.
   * @return Some path parts as strings or none if it cannot parse them.
   */
  def extractParts(path: ParsePath): Option[List[String]] = {
    prepareParsePath(path) match {
      // TODO: Support file extensions
      case ParsePath(parts, "", true, _) => Some(parts)
      case _ => None
    }
  }

  /**
   * Prepares a ParsePath for path matching in parameterForUrl.
   *
   * Calling this once and calling this multiple times is idempotent.
   */
  def prepareParsePath(path: ParsePath): ParsePath = {
    // TODO: Pass the final slash matching using ! operator
    path match {
      case p@ParsePath(Nil, _, _, true) =>
        // Can't strip anything from the path, just switch the endSlash off
        p.copy(endSlash = false)
      case p@ParsePath(pathParts, _, _, true) =>
        // Strip index page from url parts and switch the endSlash off
        p.copy(partPath = pathParts.init, endSlash = false)
      case _ => path
    }
  }

  /**
   * Determine if this Loc matches the given request.
   *
   * First, check if any PartialFunctionMatching Loc has previously matched this request
   * (even if the rewritten request path no longer matches)
   */
  abstract override def doesMatch_?(req: Req): Boolean = {
    val isMatch: Boolean = matchedLocName.get match {
      case Full(thatName) =>
        this.name == thatName
      case fail: Failure => false
      case Empty =>
        if (link.isDefinedAt(req)) {
          val locAccessible = link(req) match {
            case Full(x) if allParams.forall {
              case Loc.Test(test) => test(req)
              case _ => true
            } => x
            case Full(x) => false
            case x => x.openOr(false)
          }
          locAccessible && currentValue.isDefined
        }
        else false
    }
    if (isMatch) {
      this.onMatch()
    }
    isMatch
  }

  /**
   * A link to this Loc.
   *
   * If this link is not defined at the current request, then this Loc will not match the request.
   *
   * This not only checks that the parameterForUrl function is defined at the URL path,
   * but also that the urlForParameter is defined at the parameter extracted from the URL.
   * This is to ensure that you can't see a URL that you can't construct from this link.
   *
   * @note matchHead_? is false, because we assume that the partial function should match the whole URL.
   */
  val link = new Link[ParamsType](List(name), false) {

    /**
     * Check if the request matches this link.
     *
     * @param req The request to match.
     * @return true if the parameter partial function is defined or a redirect exception is thrown,
     *         false if the parameter partial function is not defined, the string path parts cannot be extracted,
     *         or any other type of exception is thrown.
     */
    override def isDefinedAt(req: Req): Boolean = {
      loc canHandle req.path
    }

    /**
     * Generate the URL path parts for this link.
     */
    override def pathList(value: ParamsType): List[String] = {
      urlForParameter(value)
    }
  }

  /**
   * Overridden to include any responses caught while matching the Req to this Loc.
   * @return
   */
  override def allParams: List[Loc.AnyLocParam] = capturedEarlyResponse.get match {
    case Full(resp) => EarlyResponse(resp) :: super.allParams
    case _ => super.allParams
  }

  /**
   * When the request is matched with this Loc, rewrite the request ParsePath to the templatePath.
   *
   * This is Lift's recommended way to process a template with a parameterized value from the URL.
   *
   * Before rewriting the request, this Loc's name will be stored in a RequestVar so that any other
   * PartialFunctionMatching Loc's will defer to this Loc when checking which Loc matches.
   *
   * If a non PartialFunctionMatching Loc is defined at the same request path, then
   */
  override val rewritePF: Box[LiftRules.RewritePF] =
    Full(
      NamedPF(templatePath.asFullPath) {
        new PartialFunction[RewriteRequest, RewriteResponse] {

          def isDefinedAt(req: RewriteRequest) = {
            loc.canHandle(req.path)
          }

          def apply(req: RewriteRequest): RewriteResponse = this.synchronized {
            // match the request and redirect to the template path
            matchedLocName set Full(name)
            // attempt to calculate the request value or set the early response
            calcValueOrResponse(req.path) match {
              case Left(params) =>
                requestValue set params
              case Right(resp) =>
                capturedEarlyResponse set Full(resp)
            }
            // call any post extraction hooks
            loc.postExtraction(requestValue.get)
            // stop rewriting as we have resolved to a template
            RewriteResponse(templatePath.parts.toList.map(_.slug), stopRewriting = true)
          }
        }
      }
    )
}

/**
 * Holds the first Partial Loc's name that matched the current request before any rewrites.
 */
object matchedLocName extends RequestVar[Box[String]](Empty)
