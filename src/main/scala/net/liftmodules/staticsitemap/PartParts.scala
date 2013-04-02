package net.liftmodules.staticsitemap

import net.liftweb.util.Helpers.urlEncode

class PathPartConstructionException(message: String)
extends IllegalArgumentException("Error constructing PathPart from String: " + message)

class PathPartSplitException(message: String)
extends IllegalArgumentException("Error splitting absolute path into PathParts: " + message)

sealed case class PathPart(slug: String) {
  if (slug.exists { _ == '/' })
    throw new PathPartConstructionException("PathPart cannot contain a slash: \"%s\"".format(slug))
}
sealed case class OptionalPathPart(slugOpt: Option[String]) {
  require(slugOpt.forall {
    !_.exists { _ == '/' }
  }, "PathPart cannot contain a slash.")
}
object PathPart {
  type EitherPathPart = Either[PathPart, OptionalPathPart]

  /**
   * Attempts to split the given string into a path list
   * @param absolutePath an absolute path to split
   * @throws IllegalArgumentException if the given string doesn't start with a / or contains empty path parts
   * @return The path parts as a list
   */
  def splitAbsPath(absolutePath: String): List[PathPart] = {
    if (absolutePath.charAt(0) != '/')
      throw new PathPartSplitException("Path is not absolute (does not start with '/'): \"%s\"".format(absolutePath))
    val parts = absolutePath.split('/')
    if (parts.isEmpty) Nil
    else {
      if (parts.tail.exists(_.isEmpty))
        throw new PathPartSplitException("Path contains empty parts: \"%s\"".format(absolutePath))
      else parts.tail.map{ PathPart(_) }.toList
    }
  }


  implicit def strToPathPart(part: String) = PathPart(part)
  implicit def strToOptionalPathPart(part: String) = OptionalPathPart(Some(part))
  implicit def strToEitherPathPart(part: String): EitherPathPart = Left(PathPart(part))

  implicit def optionToOptionalPathPart(part: Option[String]) = OptionalPathPart(part)
  implicit def optionToEitherPathPart(part: Option[String]): EitherPathPart = Right(OptionalPathPart(part))

  implicit def strToListPathParts(part: String) = List(part: PathPart)
  implicit def strToListOptionalPathParts(part: String) = List(part: OptionalPathPart)
  implicit def strToListEitherPathParts(part: String) = List(part: EitherPathPart)

  implicit def listStrToListPathParts(parts: List[String]) = parts map { part => part: PathPart }
  implicit def listStrToListOptionalPathParts(parts: List[String]) = parts map { part => part: OptionalPathPart }
  implicit def listStrToListEitherPathParts(parts: List[String]) = parts map { part => part: EitherPathPart }

  /* Convert between PathPart and OptionalPathPart */
  implicit def listOptionalPathPartsToListPathParts(optionalParts: List[OptionalPathPart]) = optionalParts collect {
    case OptionalPathPart(Some(slug)) => PathPart(slug)
  }

  /* Converting PathParts into List[String] */
  implicit def listPathPartsToListString(parts: List[PathPart]) = parts map {
    part => urlEncode(part.slug)
  }
  implicit def listOptionalPathPartsToListString(parts: List[OptionalPathPart]) = parts collect {
    case OptionalPathPart(Some(slug)) => urlEncode(slug)
  }
  implicit def listEitherPathPartsToListString(parts: List[EitherPathPart]) = parts collect {
    case Left(PathPart(slug)) => urlEncode(slug)
    case Right(OptionalPathPart(Some(slug))) => urlEncode(slug)
  }

}
