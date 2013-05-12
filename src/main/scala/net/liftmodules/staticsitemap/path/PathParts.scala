package net.liftmodules.staticsitemap.path

import net.liftweb.sitemap.{NormalLocPath, LocPath, *}
import net.liftmodules.staticsitemap.path.PathUtils._

case class PathParts(parts: NormalPathPart*) {
  def / (part: String): PathParts = PathParts((parts :+ NormalPathPart(part)): _*)
  def / (partOpt: Option[String]): PathParts = (partOpt: PathPart) match {
    case EmptyPathPart => this
    case part: NormalPathPart => PathParts((parts :+ part): _*)
  }
  def / (part: NormalPathPart): PathParts = PathParts((parts :+ part): _*)
  def / (part: LocPath): PathParts = PathParts((parts :+ {
    part match {
      case * => NormalPathPart("*")
      case NormalLocPath(pathItem) => NormalPathPart(pathItem)
    }
  }): _*)

  def asFullPath: String = mkFullPath(parts map {_.slug})
}
object PathParts {
  // Convert to PathParts
  implicit def listPathPartToPathParts(parts: List[NormalPathPart]): PathParts = PathParts(parts: _*)

  // Convert from PathParts
  implicit def pathPartsToListPathPart(path: PathParts): List[NormalPathPart] = path.parts.toList

  implicit def pathPartsToSeqString(path: PathParts): Seq[String] = path.parts.map{ _.slug }

  implicit def pathPartsToListString(path: PathParts): List[String] = path.parts.map{ _.slug }.toList
}
