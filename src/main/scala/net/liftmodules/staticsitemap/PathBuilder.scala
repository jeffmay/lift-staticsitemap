package net.liftmodules.staticsitemap

trait PathBuilder extends PathUtils {

  /**
   * Added to all route url matching with the / operator
   */
  protected def parts: Seq[String]

  def / (parts: Either[PathPart, OptionalPathPart]*) = mkFullPath(this.parts ++ {
    parts collect {
      case Left(PathPart(slug)) => slug
      case Right(OptionalPathPart(Some(slug))) => slug
    }
  })

  object / {
    /**
     * Convert a sequence of URI path parts into the difference of the parts given
     * from the path parts of this builder, starting from the head of each sequence.
     * @param fullPathParts The path split between the first element that doesn't match this path
     * @return The head of the given list after dropping all the same elements from this path builder
     */
    def unapplySeq(fullPathParts: List[String]) = {
      val theseElements = PathBuilder.this.parts
      if (fullPathParts startsWith theseElements) {
        val remainingElements = fullPathParts drop theseElements.length
        Some(remainingElements)
      }
      else None
    }
  }
}