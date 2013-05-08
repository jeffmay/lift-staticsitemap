package net.liftmodules.staticsitemap

/**
 * A class of exceptions for exceptions thrown by RoutesBuilders
 */
class RoutesBuilderException(msg: String = null, throwable: Throwable = null) extends Exception(msg, throwable) {
  def this(throwable: Throwable) = this(throwable.getMessage, throwable)
}