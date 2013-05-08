package net.liftmodules.staticsitemap

import net.liftweb.common.Box

trait PostExtractionHooks[ParamsType] {
  /**
   * A method that is called after the parameter is extracted from the URL.
   * @param param
   */
  def postExtraction(param: Box[ParamsType])
}
