package org.enso.languageserver.text

import org.enso.languageserver.data.ContentBasedVersioning
import org.enso.languageserver.data.buffer.Rope

/**
  * A buffer state representation.
  *
  * @param contents the contents of the buffer.
  * @param version the current version of the buffer contents.
  */
case class Buffer(contents: Rope, version: Buffer.Version)

object Buffer {
  type Version = String

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param contents the contents of this buffer.
    * @param versionCalculator a digest calculator for content based versioning.
    * @return a new buffer instance.
    */
  def apply(
    contents: Rope
  )(implicit versionCalculator: ContentBasedVersioning): Buffer =
    Buffer(contents, versionCalculator.evalVersion(contents.toString))

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param contents the contents of this buffer.
    * @param versionCalculator a digest calculator for content based versioning.
    * @return a new buffer instance.
    */
  def apply(
    contents: String
  )(implicit versionCalculator: ContentBasedVersioning): Buffer =
    Buffer(Rope(contents))
}
