package org.enso.languageserver.filemanager

import org.apache.commons.io.{FileExistsException, FileUtils}
import org.enso.languageserver.effect.BlockingIO
import zio._

import java.io.{File, FileNotFoundException, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import scala.collection.mutable
import scala.util.Using
import zio.ZIO.attemptBlocking

/** File manipulation facility.
  */
class FileSystem extends FileSystemApi[BlockingIO] {

  private val tenMb: Int = 1 * 1024 * 1024 * 10

  /** The stride used by the [[FileSystem]] when processing a file in chunks. */
  private val fileChunkSize: Int = tenMb

  import FileSystemApi._

  /** Writes textual content to a file.
    *
    * @param file path to the file
    * @param content a textual content of the file
    * @return either FileSystemFailure or Unit
    */
  override def write(
    file: File,
    content: String
  ): BlockingIO[FileSystemFailure, Unit] =
    attemptBlocking(FileUtils.write(file, content, "UTF-8"))
      .mapError(errorHandling)

  /** @inheritdoc */
  override def writeBinary(
    file: File,
    contents: Array[Byte]
  ): BlockingIO[FileSystemFailure, Unit] =
    attemptBlocking(FileUtils.writeByteArrayToFile(file, contents))
      .mapError(errorHandling)

  /** Reads the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  override def read(file: File): BlockingIO[FileSystemFailure, String] =
    attemptBlocking(FileUtils.readFileToString(file, "UTF-8"))
      .mapError(errorHandling)

  /** @inheritdoc */
  override def readBinary(
    file: File
  ): BlockingIO[FileSystemFailure, Array[Byte]] =
    attemptBlocking(FileUtils.readFileToByteArray(file))
      .mapError(errorHandling)

  /** Deletes the specified file or directory recursively.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def delete(file: File): BlockingIO[FileSystemFailure, Unit] =
    attemptBlocking {
      if (file.isDirectory) {
        FileUtils.deleteDirectory(file)
      } else {
        Files.delete(file.toPath)
      }
    }.mapError(errorHandling)

  /** Creates an empty file with parent directory.
    *
    * @param file path to the file
    * @return
    */
  override def createFile(file: File): BlockingIO[FileSystemFailure, Unit] =
    for {
      _ <- createDirectory(file.getParentFile)
      _ <- createEmptyFile(file)
    } yield ()

  private def createEmptyFile(file: File): BlockingIO[FileSystemFailure, Unit] =
    attemptBlocking(file.createNewFile(): Unit)
      .mapError(errorHandling)

  /** Creates a directory, including any necessary but nonexistent parent
    * directories.
    *
    * @param file path to the file
    * @return
    */
  override def createDirectory(
    file: File
  ): BlockingIO[FileSystemFailure, Unit] =
    attemptBlocking(FileUtils.forceMkdir(file))
      .mapError(errorHandling)

  /** Copy a file or directory recursively.
    *
    * @param from a path to the source
    * @param to a path to the destination. If from is a file, then to
    * should also be a file. If from is directory, then to should also
    * be a directory.
    * @return either [[FileSystemFailure]] or Unit
    */
  override def copy(from: File, to: File): BlockingIO[FileSystemFailure, Unit] =
    if (from.isDirectory && to.isFile) {
      ZIO.fail(FileExists)
    } else {
      attemptBlocking {
        if (from.isFile && to.isDirectory) {
          FileUtils.copyFileToDirectory(from, to)
        } else if (from.isDirectory) {
          FileUtils.copyDirectory(from, to)
        } else {
          FileUtils.copyFile(from, to)
        }
      }.mapError(errorHandling)
    }

  /** Move a file or directory recursively
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  override def move(from: File, to: File): BlockingIO[FileSystemFailure, Unit] =
    attemptBlocking {
      if (to.isDirectory) {
        val createDestDir = false
        FileUtils.moveToDirectory(from, to, createDestDir)
      } else if (from.isDirectory) {
        FileUtils.moveDirectory(from, to)
      } else {
        FileUtils.moveFile(from, to)
      }
    }.mapError(errorHandling)

  /** Checks if the specified file exists.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or file existence flag
    */
  override def exists(file: File): BlockingIO[FileSystemFailure, Boolean] =
    attemptBlocking(Files.exists(file.toPath))
      .mapError(errorHandling)

  /** List contents of a given path.
    *
    * @param path to the file system object
    * @return either [[FileSystemFailure]] or list of entries
    */
  override def list(path: File): BlockingIO[FileSystemFailure, Vector[Entry]] =
    if (path.exists) {
      if (path.isDirectory) {
        attemptBlocking {
          FileSystem
            .list(path.toPath)
            .map {
              case SymbolicLinkEntry(path, _) =>
                FileSystem.readSymbolicLink(path)
              case entry => entry
            }
        }.mapError(errorHandling)
      } else {
        ZIO.fail(NotDirectory)
      }
    } else {
      ZIO.fail(FileNotFound)
    }

  /** Returns tree of a given path.
    *
    * @param path to the file system object
    * @param depth maximum depth of a directory tree
    * @return either [[FileSystemFailure]] or directory structure
    */
  override def tree(
    path: File,
    depth: Option[Int]
  ): BlockingIO[FileSystemFailure, DirectoryEntry] = {
    val limit = FileSystem.Depth(depth)
    if (path.exists && limit.canGoDeeper) {
      if (path.isDirectory) {
        attemptBlocking {
          val directory = DirectoryEntry.empty(path.toPath)
          FileSystem.readDirectoryEntry(
            directory,
            limit.goDeeper,
            Vector(),
            mutable.Queue().appendAll(FileSystem.list(path.toPath)),
            mutable.Queue()
          )
          directory
        }.mapError(errorHandling)
      } else {
        ZIO.fail(NotDirectory)
      }
    } else {
      ZIO.fail(FileNotFound)
    }
  }

  /** Returns attributes of a given path.
    *
    * @param path to the file system object
    * @return either [[FileSystemFailure]] or file attributes
    */
  override def info(
    path: File
  ): BlockingIO[FileSystemFailure, Attributes] =
    if (path.exists) {
      attemptBlocking {
        val attrs =
          Files.readAttributes(path.toPath, classOf[BasicFileAttributes])
        Attributes.fromBasicAttributes(path.toPath, attrs)
      }.mapError(errorHandling)
    } else {
      ZIO.fail(FileNotFound)
    }

  /** Returns the digest of the file at the provided `path`
    *
    * @param path the path to the filesystem object
    * @return either [[FileSystemFailure]] or the file checksum
    */
  override def digest(path: File): BlockingIO[FileSystemFailure, SHA3_224] = {
    if (path.isFile) {
      attemptBlocking {
        val messageDigest = MessageDigest.getInstance("SHA3-224")
        Using.resource(
          Files.newInputStream(path.toPath, StandardOpenOption.READ)
        ) { stream =>
          var currentBytes = stream.readNBytes(fileChunkSize)

          while (currentBytes.nonEmpty) {
            messageDigest.update(currentBytes)
            currentBytes = stream.readNBytes(fileChunkSize)
          }

          SHA3_224(messageDigest.digest())
        }
      }.mapError(errorHandling)
    } else {
      if (path.exists()) {
        ZIO.fail(NotFile)
      } else {
        ZIO.fail(FileNotFound)
      }
    }
  }

  /** Returns the digest of the bytes described by `segment`.
    *
    * @param segment a description of the portion of a file to checksum
    * @return either [[FileSystemFailure]] or the bytes representing the checksum
    */
  override def digestBytes(
    segment: FileSegment
  ): BlockingIO[FileSystemFailure, SHA3_224] = {
    val path = segment.path
    if (path.isFile) {
      attemptBlocking {
        val messageDigest = MessageDigest.getInstance("SHA3-224")
        Using.resource(
          Files.newInputStream(path.toPath, StandardOpenOption.READ)
        ) { stream =>
          val fileLength    = Files.size(path.toPath)
          val lastByteIndex = fileLength - 1
          val lastSegIndex  = segment.byteOffset + segment.length

          if (
            segment.byteOffset > lastByteIndex || lastSegIndex > lastByteIndex
          ) {
            throw FileSystem.ReadOutOfBounds(fileLength)
          }

          var bytePosition = stream.skip(segment.byteOffset)
          var bytesToRead  = segment.length

          do {
            val readSize = Math.min(bytesToRead, fileChunkSize.toLong).toInt
            val bytes    = stream.readNBytes(readSize)

            bytePosition += bytes.length

            bytesToRead -= bytes.length

            messageDigest.update(bytes)
          } while (bytesToRead > 0)
          SHA3_224(messageDigest.digest())
        }
      }.mapError(errorHandling)
    } else {
      if (path.exists()) {
        ZIO.fail(NotFile)
      } else {
        ZIO.fail(FileNotFound)
      }
    }
  }

  override def writeBytes(
    path: File,
    byteOffset: Long,
    overwriteExisting: Boolean,
    bytes: Array[Byte]
  ): BlockingIO[FileSystemFailure, SHA3_224] = {
    if (path.isDirectory) {
      ZIO.fail(NotFile)
    } else {
      attemptBlocking {
        Using.resource(new RandomAccessFile(path, "rw")) { file =>
          Using.resource(file.getChannel) { chan =>
            val lock = chan.lock()
            try {
              val fileSize = chan.size()

              val messageDigest = MessageDigest.getInstance("SHA3-224")

              if (byteOffset < fileSize) {
                if (overwriteExisting) {
                  chan.truncate(byteOffset)
                } else {
                  throw FileSystem.CannotOverwrite
                }
              } else if (byteOffset > fileSize) {
                chan.position(fileSize)
                var nullBytesLeft = byteOffset - fileSize

                do {
                  val numBytesInRound =
                    Math.min(nullBytesLeft, fileChunkSize.toLong)
                  val bytes    = Array.fill(numBytesInRound.toInt)(0x0.toByte)
                  val bytesBuf = ByteBuffer.wrap(bytes)
                  messageDigest.update(bytes)
                  chan.write(bytesBuf)

                  nullBytesLeft -= numBytesInRound
                } while (nullBytesLeft > 0)
              }

              chan.position(chan.size())
              messageDigest.update(bytes)
              chan.write(ByteBuffer.wrap(bytes))

              SHA3_224(messageDigest.digest())
            } finally {
              lock.release()
            }
          }
        }
      }.mapError(errorHandling)
    }
  }

  override def readBytes(
    segment: FileSegment
  ): BlockingIO[FileSystemFailure, ReadBytesResult] = {
    val path = segment.path
    if (path.isFile) {
      attemptBlocking {
        Using.resource(
          Files.newInputStream(path.toPath, StandardOpenOption.READ)
        ) { stream =>
          stream.skip(segment.byteOffset)
          val fileSize      = Files.size(path.toPath)
          val lastByteIndex = fileSize - 1

          if (lastByteIndex < segment.byteOffset) {
            throw FileSystem.ReadOutOfBounds(fileSize)
          }

          val bytesToRead = segment.length
          val bytes       = stream.readNBytes(bytesToRead.toInt)

          val digest = MessageDigest.getInstance("SHA3-224").digest(bytes)

          ReadBytesResult(SHA3_224(digest), bytes)
        }
      }.mapError(errorHandling)
    } else {
      if (path.exists()) {
        ZIO.fail(NotFile)
      } else {
        ZIO.fail(FileNotFound)
      }
    }
  }

  private val errorHandling: Throwable => FileSystemFailure = {
    case _: FileNotFoundException      => FileNotFound
    case _: NoSuchFileException        => FileNotFound
    case _: FileExistsException        => FileExists
    case _: AccessDeniedException      => AccessDenied
    case FileSystem.ReadOutOfBounds(l) => ReadOutOfBounds(l)
    case FileSystem.CannotOverwrite    => CannotOverwrite
    case ex                            => GenericFileSystemFailure(ex.getMessage)
  }
}

object FileSystem {

  /** An exception for when a file segment read goes out of bounds.
    *
    * @param length the true length of the file
    */
  case class ReadOutOfBounds(length: Long) extends Throwable

  /** An exception for when overwriting would be required but the corresponding
    * flag is not set.
    */
  case object CannotOverwrite extends Throwable

  import FileSystemApi._

  /** Represent a depth limit when recursively traversing a directory.
    */
  sealed private trait Depth {

    def canGoDeeper: Boolean

    def goDeeper: Depth
  }

  private object Depth {

    def apply(depth: Option[Int]): Depth =
      depth.fold[Depth](UnlimitedDepth)(LimitedDepth)
  }

  private case class LimitedDepth(limit: Int) extends Depth {

    override def canGoDeeper: Boolean =
      limit > 0

    override def goDeeper: Depth =
      LimitedDepth(limit - 1)
  }

  private case object UnlimitedDepth extends Depth {

    override def canGoDeeper: Boolean =
      true

    override def goDeeper: Depth =
      UnlimitedDepth
  }

  /** Represents subdirectory in the tree algorithm.
    *
    * @param entry subdir entry
    * @param level subdir depth level
    * @param visited list of visited symlinks
    */
  private case class Subdir(
    entry: DirectoryEntry,
    level: Depth,
    visited: Vector[SymbolicLinkEntry]
  )

  /** Read an entry without following the symlinks.
    */
  private def readEntry(path: Path): Entry = {
    if (Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
      FileEntry(path)
    } else if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
      DirectoryEntryTruncated(path)
    } else if (Files.isSymbolicLink(path)) {
      val target = Files.readSymbolicLink(path)
      if (Files.exists(target)) {
        SymbolicLinkEntry(path, target)
      } else {
        OtherEntry(path)
      }
    } else {
      OtherEntry(path)
    }
  }

  /** Read the target of a symlink.
    */
  private def readSymbolicLink(path: Path): Entry = {
    if (Files.isRegularFile(path)) {
      FileEntry(path)
    } else if (Files.isDirectory(path)) {
      DirectoryEntryTruncated(path)
    } else {
      OtherEntry(path)
    }
  }

  /** Returns the entries of the provided path. Symlinks are not resolved.
    *
    * @param path to the directory
    * @return list of entries
    */
  private def list(path: Path): Vector[Entry] = {
    def accumulator(acc: Vector[Entry], path: Path): Vector[Entry] =
      acc :+ readEntry(path)
    def combiner(as: Vector[Entry], bs: Vector[Entry]): Vector[Entry] =
      as ++ bs
    Files
      .list(path)
      .reduce(Vector(), accumulator, combiner)
      .sortBy(_.path)
  }

  /** Makes BFS traversal and updates provided directory [[DirectoryEntry]].
    * Symlinks are resolved. Returned [[SymbolicLinkEntry]] indicates a loop.
    *
    * @param directory current directory
    * @param level a maximum depth of the directory tree
    * @param visited symlinked directories
    * @param entryQueue unprocessed directory entries
    * @param subdirQueue unprocessed subdirectories
    */
  @scala.annotation.tailrec
  private def readDirectoryEntry(
    directory: DirectoryEntry,
    level: Depth,
    visited: Vector[SymbolicLinkEntry],
    entryQueue: mutable.Queue[Entry],
    subdirQueue: mutable.Queue[Subdir]
  ): Unit = {
    if (entryQueue.isEmpty) {
      // done with the current directory, sort children and go to the next one
      directory.children.sortInPlaceBy(_.path)
      if (subdirQueue.nonEmpty) {
        val subdir = subdirQueue.dequeue()
        readDirectoryEntry(
          subdir.entry,
          subdir.level,
          subdir.visited,
          mutable.Queue().appendAll(list(subdir.entry.path)),
          subdirQueue
        )
      }
    } else {
      // process next entry
      entryQueue.dequeue() match {
        case DirectoryEntryTruncated(path) =>
          if (level.canGoDeeper) {
            // can go deeper, enqueue subdirectory and continue
            val subdir = DirectoryEntry.empty(path)
            directory.children.append(subdir)
            subdirQueue.enqueue(Subdir(subdir, level.goDeeper, visited))
            readDirectoryEntry(
              directory,
              level,
              visited,
              entryQueue,
              subdirQueue
            )
          } else {
            // can't go deeper, add truncated directory to children and continue
            directory.children.append(DirectoryEntryTruncated(path))
            readDirectoryEntry(
              directory,
              level,
              visited,
              entryQueue,
              subdirQueue
            )
          }
        case symlink @ SymbolicLinkEntry(path, target) =>
          if (Files.isDirectory(target, LinkOption.NOFOLLOW_LINKS)) {
            visited.find(_.target == target) match {
              case Some(SymbolicLinkEntry(visitedPath, _)) =>
                // symlink has been already visited, break the loop
                directory.children.append(SymbolicLinkEntry(path, visitedPath))
                readDirectoryEntry(
                  directory,
                  level,
                  visited,
                  entryQueue,
                  subdirQueue
                )
              case None =>
                // add symlink to visited, enqueue resolved symlink and continue
                entryQueue.enqueue(readSymbolicLink(path))
                readDirectoryEntry(
                  directory,
                  level,
                  visited :+ symlink,
                  entryQueue,
                  subdirQueue
                )
            }
          } else {
            // symlink is not a directory, enqueue resolved symlink and continue
            entryQueue.enqueue(readSymbolicLink(path))
            readDirectoryEntry(
              directory,
              level,
              visited,
              entryQueue,
              subdirQueue
            )
          }
        case entry =>
          // add entry and continue
          directory.children.append(entry)
          readDirectoryEntry(
            directory,
            level,
            visited,
            entryQueue,
            subdirQueue
          )
      }
    }
  }
}
