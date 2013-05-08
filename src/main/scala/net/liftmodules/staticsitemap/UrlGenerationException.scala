package net.liftmodules.staticsitemap

/**
 * Exception thrown when a URL cannot be constructed with the parameter passed to the url() method of a Loc.
 * @param routeName
 * @param param
 */
class UrlGenerationException(
  routeName: String,
  param: Any)
  extends Exception {

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
