package org.enso.projectmanager.service.filesystem

import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.time.{OffsetDateTime, ZoneOffset}

/** Basic attributes of a filesystem entry.
  *
  * @param creationTime creation time
  * @param lastAccessTime last access time
  * @param lastModifiedTime last modified time
  * @param byteSize size of entry in bytes
  */
case class Attributes(
  creationTime: OffsetDateTime,
  lastAccessTime: OffsetDateTime,
  lastModifiedTime: OffsetDateTime,
  byteSize: Long
)

object Attributes {

  /** Create attributes from [[BasicFileAttributes]].
    *
    * @param basicFileAttributes basic file attributes
    * @return basic attributes of a filesystem entry
    */
  def apply(basicFileAttributes: BasicFileAttributes): Attributes =
    new Attributes(
      utcTime(basicFileAttributes.creationTime()),
      utcTime(basicFileAttributes.lastAccessTime()),
      utcTime(basicFileAttributes.lastModifiedTime()),
      basicFileAttributes.size()
    )

  private def utcTime(time: FileTime): OffsetDateTime =
    OffsetDateTime.ofInstant(time.toInstant, ZoneOffset.UTC)

}
