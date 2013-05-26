package net.liftmodules

package object staticsitemap {

  class MissingRoutesException[Sitemap, Expected](implicit sitemap: Manifest[Sitemap], expected: Manifest[Expected])
    extends Exception("Could not cast the SiteMap type %s as %s".format(
      sitemap.runtimeClass.getSimpleName, expected.runtimeClass.getSimpleName)
    )

}
