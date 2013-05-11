package net.liftmodules.staticsitemap.path

trait PathExtracting {
  def extract: PartialFunction[List[String], List[String]]
}

