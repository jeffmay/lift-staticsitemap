package net.liftmodules.staticsitemap

package object path {
  trait PathPartException {
    this: Exception =>
  }

  class PathPartConstructionException(message: String)
    extends IllegalArgumentException("Error constructing PathPart from String: " + message)
    with PathPartException

  class PathPartSplitException(message: String)
    extends IllegalArgumentException("Error splitting absolute path into PathParts: " + message)
    with PathPartException
}
