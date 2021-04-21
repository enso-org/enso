package org.enso.runtimeversionmanager.releases

/** Indicates a release provider failure. */
sealed class ReleaseProviderException(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {

  /** @inheritdoc */
  override def toString: String =
    s"A problem occurred when trying to find the release: $message"
}

object ReleaseProviderException {

  /** Creates a release provider exception with a given message and optional
    * cause.
    */
  def apply(
    message: String,
    cause: Throwable = null
  ): ReleaseProviderException = new ReleaseProviderException(message, cause)
}

/** Indicates that the specific release could not be found.
  *
  * @param tag the tag of the release
  * @param cause (optional) an exception that has caused this error
  */
case class ReleaseNotFound(
  tag: String,
  message: Option[String] = None,
  cause: Throwable        = null
) extends ReleaseProviderException(
      message.getOrElse(s"Cannot find release `$tag`."),
      cause
    )
