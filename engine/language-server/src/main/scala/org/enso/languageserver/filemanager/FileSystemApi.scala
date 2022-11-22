package org.enso.languageserver.filemanager

import java.io.File
import java.nio.file.Path
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.time.{OffsetDateTime, ZoneOffset}
import org.enso.languageserver.effect.BlockingIO

import scala.collection.mutable.ArrayBuffer

/** File manipulation API.
  *
  * @tparam F represents target monad
  */
trait FileSystemApi[F[_, _]] {

  import FileSystemApi._

  /** Writes textual content to a file.
    *
    * @param file path to the file
    * @param content a textual content of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def write(
    file: File,
    content: String
  ): F[FileSystemFailure, Unit]

  /** Writes binary content to a file.
    *
    * @param file path to the file
    * @param contents a binary content of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def writeBinary(
    file: File,
    contents: Array[Byte]
  ): BlockingIO[FileSystemFailure, Unit]

  /** Reads the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  def read(file: File): F[FileSystemFailure, String]

  /** Reads the contents of a binary file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a byte
    *         array
    */
  def readBinary(file: File): BlockingIO[FileSystemFailure, Array[Byte]]

  /** Deletes the specified file or directory recursively.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def delete(file: File): F[FileSystemFailure, Unit]

  /** Creates an empty file with parent directory.
    *
    * @param file path to the file
    * @return
    */
  def createFile(file: File): F[FileSystemFailure, Unit]

  /** Creates a directory, including any necessary but nonexistent parent
    * directories.
    *
    * @param file path to the file
    * @return
    */
  def createDirectory(file: File): F[FileSystemFailure, Unit]

  /** Copy a file or directory recursively
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def copy(
    from: File,
    to: File
  ): F[FileSystemFailure, Unit]

  /** Move a file or directory recursively
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def move(
    from: File,
    to: File
  ): F[FileSystemFailure, Unit]

  /** Checks if the specified file exists.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or file existence flag
    */
  def exists(file: File): F[FileSystemFailure, Boolean]

  /** List contents of a given path.
    *
    * @param path to the file system object
    * @return either [[FileSystemFailure]] or list of entries
    */
  def list(path: File): F[FileSystemFailure, Vector[Entry]]

  /** Returns contents of a given path.
    *
    * @param path to the file system object
    * @param depth maximum depth of a directory tree
    * @return either [[FileSystemFailure]] or directory structure
    */
  def tree(
    path: File,
    depth: Option[Int]
  ): F[FileSystemFailure, DirectoryEntry]

  /** Returns attributes of a given path.
    *
    * @param path to the file system object
    * @return either [[FileSystemFailure]] or file attributes
    */
  def info(path: File): F[FileSystemFailure, Attributes]

  /** Returns the digest for the file at the provided path.
    *
    * @param path the path to the filesystem object
    * @return either [[FileSystemFailure]] or the file checksum
    */
  def digest(path: File): F[FileSystemFailure, SHA3_224]

  /** Returns the digest for the bytes in the file described by `segment`.
    *
    * @param segment a description of the portion of a file to checksum
    * @return either [[FileSystemFailure]] or the bytes representing the checksum
    */
  def digestBytes(segment: FileSegment): F[FileSystemFailure, SHA3_224]

  /** Writes the provided `bytes` to the file at `path` on disk.
    *
    * @param path the path to the file into which the bytes will be written
    * @param byteOffset the offset in the file to start writing from
    * @param overwriteExisting whether existing bytes can be overwritten
    * @param bytes the bytes to write to the file
    * @return either a [[FileSystemFailure]] or the checksum of the `bytes` as
    *         they were written to disk
    */
  def writeBytes(
    path: File,
    byteOffset: Long,
    overwriteExisting: Boolean,
    bytes: Array[Byte]
  ): F[FileSystemFailure, SHA3_224]

  /** Reads the bytes specified by `segment` from the specified `segment.file`.
    *
    * @param segment a description of the portion of a file to checksum
    * @return either [[FileSystemFailure]] or the bytes representing the checksum
    */
  def readBytes(segment: FileSegment): F[FileSystemFailure, ReadBytesResult]
}

object FileSystemApi {

  /** A SHA3-224 digest on the filesystem.
    *
    * @param bytes the bytes that represent the value of the digest
    */
  case class SHA3_224(bytes: Array[Byte])

  /** The bytes read from the file.
    *
    * @param checksum the checksum of `bytes`
    * @param bytes the bytes that were read
    */
  case class ReadBytesResult(checksum: SHA3_224, bytes: Array[Byte])

  /** A representation of a segment in the file.
    *
    * @param path the path to the file in question
    * @param byteOffset the byte offset in the file to start from
    * @param length the number of bytes in the segment
    */
  case class FileSegment(path: File, byteOffset: Long, length: Long)

  /** An object representing abstract file system entry.
    */
  sealed trait Entry {
    def path: Path
  }

  object Entry {

    /** Creates [[Entry]] from file system attributes.
      *
      * @param path a path to the file system object
      * @param attrs a file system attributes
      * @return an entry
      */
    def fromBasicAttributes(path: Path, attrs: BasicFileAttributes): Entry =
      if (attrs.isDirectory) {
        DirectoryEntryTruncated(path)
      } else if (attrs.isRegularFile) {
        FileEntry(path)
      } else {
        OtherEntry(path)
      }
  }

  /** An entry representing a directory.
    *
    * @param path to the directory
    * @param children a paths to the children entries
    */
  case class DirectoryEntry(path: Path, children: ArrayBuffer[Entry])
      extends Entry

  object DirectoryEntry {

    def empty(path: Path): DirectoryEntry =
      DirectoryEntry(path, ArrayBuffer())
  }

  /** An entry representing a directory with contents truncated.
    *
    * @param path to the directory
    */
  case class DirectoryEntryTruncated(path: Path) extends Entry

  /** An entry representing a symbolic link.
    *
    * @param path to the symlink
    * @param target of the symlink.
    */
  case class SymbolicLinkEntry(path: Path, target: Path) extends Entry

  /** An entry representing a file.
    *
    * @param path to the file
    */
  case class FileEntry(path: Path) extends Entry

  /** Unrecognized file system entry. Example is a broken symlink.
    */
  case class OtherEntry(path: Path) extends Entry

  /** Basic attributes of an [[Entry]].
    *
    * @param creationTime creation time
    * @param lastAccessTime last access time
    * @param lastModifiedtime last modified time
    * @param kind either [[DirectoryEntryTruncated]] or [[FileEntry]] or [[OtherEntry]]
    * @param byteSize size of entry in bytes
    */
  case class Attributes(
    creationTime: OffsetDateTime,
    lastAccessTime: OffsetDateTime,
    lastModifiedTime: OffsetDateTime,
    kind: Entry,
    byteSize: Long
  )

  object Attributes {

    /** Creates attributes using the `FileTime` time.
      *
      * @param creationTime creation time
      * @param lastAccessTime last access time
      * @param lastModifiedtime last modified time
      * @param kind a type of the file system object
      * @param byteSize size of an entry in bytes
      * @return file attributes
      */
    def apply(
      creationTime: FileTime,
      lastAccessTime: FileTime,
      lastModifiedTime: FileTime,
      kind: Entry,
      byteSize: Long
    ): Attributes =
      Attributes(
        creationTime     = utcTime(creationTime),
        lastAccessTime   = utcTime(lastAccessTime),
        lastModifiedTime = utcTime(lastModifiedTime),
        kind             = kind,
        byteSize         = byteSize
      )

    /** Creates [[Attributes]] from file system attributes
      *
      * @param path to the file system object
      * @param attributes of a file system object
      * @return file attributes
      */
    def fromBasicAttributes(
      path: Path,
      basic: BasicFileAttributes
    ): Attributes =
      Attributes(
        creationTime     = basic.creationTime(),
        lastAccessTime   = basic.lastAccessTime(),
        lastModifiedTime = basic.lastModifiedTime(),
        kind             = Entry.fromBasicAttributes(path, basic),
        byteSize         = basic.size()
      )

    private def utcTime(time: FileTime): OffsetDateTime =
      OffsetDateTime.ofInstant(time.toInstant, ZoneOffset.UTC)
  }
}
