package org.enso.languageserver.text

import java.io.File

import org.enso.languageserver.data.ContentBasedVersioning
import org.enso.text.buffer.Rope

/**
  * A buffer state representation.
  *
  * @param file the file linked to the buffer.
  * @param contents the contents of the buffer.
  * @param version the current version of the buffer contents.
  */
case class Buffer(file: File, contents: Rope, version: Buffer.Version)

object Buffer {
  type Version = String

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param file the file linked to the buffer.
    * @param contents the contents of this buffer.
    * @param versionCalculator a digest calculator for content based versioning.
    * @return a new buffer instance.
    */
  def apply(
    file: File,
    contents: Rope
  )(implicit versionCalculator: ContentBasedVersioning): Buffer =
    Buffer(file, contents, versionCalculator.evalVersion(contents.toString))

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param file the file linked to the buffer.
    * @param contents the contents of this buffer.
    * @param versionCalculator a digest calculator for content based versioning.
    * @return a new buffer instance.
    */
  def apply(
    file: File,
    contents: String
  )(implicit versionCalculator: ContentBasedVersioning): Buffer =
    Buffer(file, Rope(contents))
}
