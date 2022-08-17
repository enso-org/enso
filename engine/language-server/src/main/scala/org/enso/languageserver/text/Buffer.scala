package org.enso.languageserver.text

import java.io.File

import org.enso.text.{ContentBasedVersioning, ContentVersion}
import org.enso.text.buffer.Rope

/** A buffer state representation.
  *
  * @param file the file linked to the buffer.
  * @param contents the contents of the buffer.
  * @param inMemory determines if the buffer is in-memory
  * @param version the current version of the buffer contents.
  */
case class Buffer(
  file: File,
  contents: Rope,
  inMemory: Boolean,
  version: ContentVersion
)

object Buffer {

  /** Creates a new buffer with a freshly generated version.
    *
    * @param file the file linked to the buffer.
    * @param contents the contents of this buffer.
    * @param inMemory determines if the buffer is in-memory
    * @param versionCalculator a digest calculator for content based versioning.
    * @return a new buffer instance.
    */
  def apply(
    file: File,
    contents: Rope,
    inMemory: Boolean
  )(implicit versionCalculator: ContentBasedVersioning): Buffer =
    Buffer(
      file,
      contents,
      inMemory,
      versionCalculator.evalVersion(contents.toString)
    )

  /** Creates a new buffer with a freshly generated version.
    *
    * @param file the file linked to the buffer.
    * @param contents the contents of this buffer.
    * @param inMemory determines if the buffer is in-memory
    * @param versionCalculator a digest calculator for content based versioning.
    * @return a new buffer instance.
    */
  def apply(
    file: File,
    contents: String,
    inMemory: Boolean
  )(implicit versionCalculator: ContentBasedVersioning): Buffer =
    Buffer(file, Rope(contents), inMemory)
}
