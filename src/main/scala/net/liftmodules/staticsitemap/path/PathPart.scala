package net.liftmodules.staticsitemap.path

import net.liftweb.util.Helpers._
import net.liftweb.sitemap.{*, LocPath}

case class NormalPathPart(slug: String) extends PathPart(slug, false)

case object EmptyPathPart extends PathPart("", true)

sealed abstract class PathPart(val slugOpt: Option[String], val optional: Boolean) {
  if (!optional && slugOpt.isEmpty)
    throw new PathPartConstructionException("Non optional PathPart cannot be empty: " + this.toString)
  if (slugOpt exists { _ exists { _ == '/' } })
    throw new PathPartConstructionException("PathPart cannot contain a slash.")

  def this(slug: String, optional: Boolean) = this(if (slug.isEmpty) None else Some(slug), optional)
}

object PathPart {

  def apply(slug: String): PathPart = if (slug.isEmpty) EmptyPathPart else NormalPathPart(slug)

  def apply(slug: LocPath): PathPart = if (slug.wildcard_?) apply("*") else apply(slug.pathItem)

  def unapply(o: Any): Option[Option[String]] = o match {
    case part: NormalPathPart => Some(part.slugOpt)
    case _ => None
  }

  /**
   * Attempts to split the given string into a path list
   * @param absolutePath an absolute path to split
   * @throws IllegalArgumentException if the given string doesn't start with a / or contains empty path parts
   * @return The path parts as a list
   */
  def splitAbsPath(absolutePath: String): List[NormalPathPart] = {
    if (absolutePath.charAt(0) != '/')
      throw new PathPartSplitException("Path is not absolute (does not start with '/'): \"%s\"".format(absolutePath))
    val parts = absolutePath.split('/')
    if (parts.isEmpty) Nil
    else parts.tail.map{ NormalPathPart(_) }.toList
  }

  implicit def strToNormalPathPart(part: String): NormalPathPart = NormalPathPart(part)

  implicit def locPathToPathPart(part: LocPath): NormalPathPart = NormalPathPart(if (part == *) "*" else part.pathItem)

  implicit def optionStringToPathPart(part: Option[String]): PathPart = PathPart(part.getOrElse(""))

  implicit def strToListNormalPathParts(part: String): List[NormalPathPart] = List(NormalPathPart(part))

  implicit def locPathToListNormalPathPart(part: LocPath): List[NormalPathPart] = List(part)

  implicit def seqStrToListNormalPathParts(parts: Seq[String]): List[NormalPathPart] =
    (parts map { part => NormalPathPart(part) }).toList

  /* Convert between PathPart and OptionalPathPart */
  implicit def listPathPartsToListNormalPathParts(optionalParts: List[PathPart]): List[NormalPathPart] =
    optionalParts collect {
      case part: NormalPathPart => part
    }

  /* Converting PathParts into List[String] */
  implicit def normalPathPartToString(part: NormalPathPart) = part.slug
  implicit def seqPathPartsToListString(parts: Seq[PathPart]): Seq[String] =
    parts map { _.slugOpt.getOrElse("") }
  implicit def listPathPartsToListString(parts: List[PathPart]): List[String] =
    parts map { _.slugOpt.getOrElse("") }
  implicit def listNormalPathPartsToListString(parts: List[NormalPathPart]): List[String] =
    parts map {
      part => urlEncode(part.slug)
    }

}