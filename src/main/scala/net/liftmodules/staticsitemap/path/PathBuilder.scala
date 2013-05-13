package net.liftmodules.staticsitemap.path

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait PathBuilder {

  /**
   * Added to all route url matching with the / operator
   */
  protected def parts: Seq[NormalPathPart]

  /**
   * The number of wildcards in the parts
   */
  private lazy val wildcards = parts count {_.slug == "*"}

  /**
   * The root path "/"
   */
  val ^ : Seq[String] = Queue.empty

  /**
   * PathParts object containing all the parts built up to the point of the first wildcard.
   */
  lazy val ^** : Seq[String] = Queue({
    parts takeWhile {
      _.slug != "*"
    } map {
      _.slug
    }
  }: _*)

  /**
   * Convert a List of path parts into a PathParts object
   */
  implicit def toPathParts(parts: Seq[Any]) = PathParts(parts collect {
    case part: String => NormalPathPart(part)
    case part: NormalPathPart => part
    case PathPart(Some(slug)) => NormalPathPart(slug)
  }: _*)


  /**
   * Convert some PathParts object into a full url
   */
  implicit def pathPartsToString(path: PathParts): String = path.asFullPath

  /**
   * Pattern builder that inserts path parts into any wildcards
   */
  object % {

    /**
     * Extract the parts of the path that are located at a * wildcard part of the path.
     *
     * Ex:
     * {{{
     *   builder = new PathBuilder {
     *     val parts = ^ / "one" / *
     *
     *     List("one", "two") match {
     *       case %(param) =>  // param == "two"
     *     }
     *   }
     * }}}
     *
     * @param incoming The list of string path parts to extract from
     * @return The matched wildcard items in order from left to right.
     */
    def unapplySeq(incoming: List[String]): Option[Seq[String]] = {
      @tailrec
      def fillWildcards(
        incoming: List[NormalPathPart],
        parts: List[NormalPathPart],
        filled: List[String]): Option[List[String]] =
        parts match {
          case Nil if (incoming.isEmpty) =>
            if (filled.size == wildcards)
            // If we have matched the number of wildcards we were expecting
              Some(filled.reverse)
            else
              None
          case NormalPathPart("*") :: remainingParts if !incoming.isEmpty =>
            // Add the head from the incoming path parts matched by the wildcard in the order it appears
            fillWildcards(incoming.tail, remainingParts, incoming.head.slug :: filled)
          case nextPart :: remainingParts if (!incoming.isEmpty && nextPart == incoming.head) =>
            // Don't match anything but continue processing
            fillWildcards(incoming.tail, remainingParts, filled)
          case _ =>
            // Wrong size or unmatched constant
            None
        }
      val ret = fillWildcards(incoming, parts.toList, Nil)
      ret
    }
  }

  /**
   * Pattern extractor that splits a list of path part strings so that it can be matched using PathParts objects.
   */
  object / {

    def unapply(path: Seq[String]): Option[(Seq[String], String)] = path match {
      case Nil => None
      case parts =>
        Some(parts.toVector.init, parts.last)
    }

  }
}

object PathBuilder extends PathBuilder {

  /**
   * Added to all route url matching with the / operator
   */
  protected val parts = Nil

}
