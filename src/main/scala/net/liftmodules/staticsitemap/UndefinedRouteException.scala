package net.liftmodules.staticsitemap

/**
 * Exception thrown when a route cannot be found with the specified name.
 * @param routeName The name of the route that was searched for
 * @param withParameter Whether or not the route was expected to have a parameter
 */
class UndefinedRouteException(
  routeName: String,
  withParameter: Boolean)
  extends Exception {

  override def getMessage(): String = {
    if (withParameter)
      """Route "%s" does not exist in the sitemap.""".format(routeName)
    else
      """Parameterless route "%s" does not exist in the sitemap.""".format(routeName)
  }
}
