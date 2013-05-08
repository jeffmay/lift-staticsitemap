package net.liftmodules.staticsitemap.path

import PathUtils._
import scala.annotation.tailrec

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
   * PathParts object containing all the parts built up to the point of the first wildcard.
   */
  case object ^** extends PathParts(
    parts takeWhile {
      _.slug != "*"
    }: _*)

  /**
   * Convert some PathParts object into a full url
   */
  implicit def pathPartsToString(path: PathParts): String = mkFullPath(path.parts.toSeq)

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

    def unapply(path: List[String]): Option[(PathParts, String)] = path match {
      case Nil => None
      case parts =>
        val init = PathParts(parts.init: _*)
        val leading = if (^**.parts == init.parts) ^** else init
        Some(leading, parts.last)
    }

    def unapply(mid: PathParts): Option[(PathParts, String)] = mid.parts match {
      case Seq() => None
      case q: Seq[NormalPathPart] =>
        val init = PathParts(q.init: _*)
        val leading = if (^**.parts == init.parts) ^** else init
        Some(leading, q.last.slug)
    }

  }

}