package net.liftmodules.staticsitemap.path

import net.liftweb.sitemap.{NormalLocPath, LocPath, *}
import net.liftmodules.staticsitemap.path.PathUtils._

case class PathParts(parts: NormalPathPart*) {
  def / (part: String): PathParts = PathParts(parts :+ NormalPathPart(part): _*)
  def / (partOpt: Option[String]): PathParts = (partOpt: PathPart) match {
    case EmptyPathPart => this
    case part: NormalPathPart => PathParts(parts :+ part: _*)
  }
  def / (part: NormalPathPart): PathParts = PathParts(parts :+ part: _*)
  def / (part: LocPath): PathParts = PathParts(parts :+ {
    part match {
      case * => NormalPathPart("*")
      case NormalLocPath(pathItem) => NormalPathPart(pathItem)
    }
  }: _*)
  def / (that: PathParts): PathParts = PathParts(this.parts ++ that.parts: _*)

  def asFullPath: String = mkFullPath(parts.map(_.slug))
}

object PathParts {

  /**
   * Attempts to split the given string into a path list
   * @param absolutePath an absolute path to split
   * @throws IllegalArgumentException if the given string doesn't start with a / or contains empty path parts
   * @return The path parts as a list
   */
  def fromAbsPath(absolutePath: String): PathParts = {
    if (absolutePath.charAt(0) != '/')
      throw new PathPartSplitException("Path is not absolute (does not start with '/'): \"%s\"".format(absolutePath))
    val parts = absolutePath.split('/')
    if (parts.isEmpty) PathParts()
    else PathParts(parts.tail map NormalPathPart: _*)
  }

  // Convert from PathParts
  implicit def pathPartsToListPathPart(path: PathParts): List[NormalPathPart] = path.parts.toList

  implicit def pathPartsToSeqString(path: PathParts): Seq[String] = path.parts.map(_.slug)

  implicit def pathPartsToListString(path: PathParts): List[String] = path.parts.map(_.slug).toList
}
