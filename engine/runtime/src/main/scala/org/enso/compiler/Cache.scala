package org.enso.compiler

import com.oracle.truffle.api.{TruffleFile, TruffleLogger}
import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex
import org.enso.interpreter.runtime.EnsoContext
import org.enso.logger.masking.MaskedPath

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  IOException,
  ObjectInputStream,
  ObjectOutputStream
}
import java.nio.file.{
  FileAlreadyExistsException,
  NoSuchFileException,
  Path,
  StandardOpenOption
}
import java.security.MessageDigest
import java.util.logging.Level
import scala.util.{Failure, Success, Try, Using}

/** Responsible for the management of caches associated with a given data.
  * @tparam T the type of the underlying cached data entry
  */
trait Cache[T] {

  def logLevel: Level

  import Cache._

  /** Type of Metadata representation corresponding to the given data
    */
  type M <: Metadata

  /** Saves the provided IR into the cache for this module.
    *
    * @param entry the data representation to be saved
    * @param context the language context in which saving is taking place
    * @param useGlobalCacheLocations if true, will use global caches' location, local one otherwise
    * @return returns the location of the cache if successful, and [[None]] if
    *         it was unable to save
    */
  def save(
    entry: T,
    context: EnsoContext,
    useGlobalCacheLocations: Boolean
  ): Option[TruffleFile] = {
    implicit val logger: TruffleLogger = context.getLogger(this.getClass)
    getCacheRoots(context) match {
      case Some(roots) =>
        if (useGlobalCacheLocations) {
          if (saveCacheTo(roots.globalCacheRoot, entry)) {
            return Some(roots.globalCacheRoot)
          }
        } else {
          logger.log(
            logLevel,
            s"Skipping use of global cache locations for $stringRepr."
          )
        }

        if (saveCacheTo(roots.localCacheRoot, entry)) {
          return Some(roots.localCacheRoot)
        }

        logger.log(
          logLevel,
          s"Unable to write cache data for $stringRepr."
        )
        None
      case None => None
    }
  }

  protected def computeDigest(entry: T): Option[String]

  protected def computeDigestFromSource(context: EnsoContext): Option[String]

  protected def getCacheRoots(context: EnsoContext): Option[Roots]

  protected def extractObjectToSerialize(entry: T): Object

  /** Saves the cache into the provided `cacheRoot`.
    *
    * @param cacheRoot the root into which the cache should be saved
    * @param entry    the data that should be cached
    * @param logger   a logger
    * @return `true` if the cache was written successfully, `false` otherwise
    */
  private def saveCacheTo(
    cacheRoot: TruffleFile,
    entry: T
  )(implicit logger: TruffleLogger): Boolean = {
    if (ensureRoot(cacheRoot)) {
      val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val bytesToWrite =
        Using.resource(new ObjectOutputStream(byteStream)) { stream =>
          stream.writeObject(extractObjectToSerialize(entry))
          byteStream.toByteArray
        }

      val blobDigest = computeDigestFromBytes(bytesToWrite)
      val sourceDigest = computeDigest(entry).getOrElse(
        throw new IllegalStateException("unable to compute digest")
      )
      val metadataBytes = metadata(sourceDigest, blobDigest, entry)

      val cacheDataFile = getCacheDataPath(cacheRoot)
      val metadataFile  = getCacheMetadataPath(cacheRoot)
      val parentPath    = cacheDataFile.getParent

      if (writeBytesTo(cacheDataFile, bytesToWrite)) {
        if (writeBytesTo(metadataFile, metadataBytes)) {
          logger.log(
            logLevel,
            s"Written cache data [$stringRepr] to [${parentPath.toMaskedPath.applyMasking()}]."
          )
          return true
        } else {
          // Clean up after ourselves if it fails.
          cacheDataFile.delete()
        }
      }

      return true
    }

    false
  }

  /** Loads the cache for this data where possible.
    *
    * @param context the language context in which loading is taking place
    * @return the cached data if possible, and [[None]] if it could not
    *         load a valid cache
    */
  def load(context: EnsoContext): Option[T] = {
    this.synchronized {
      implicit val logger: TruffleLogger = context.getLogger(this.getClass)
      logger.log(logLevel, "loading from Cache")
      getCacheRoots(context) match {
        case Some(roots) =>
          logger.log(logLevel, "got roots: " + roots)
          // Load from the global root as a priority.
          loadCacheFrom(roots.globalCacheRoot, context) match {
            case cache @ Some(_) =>
              logger.log(
                logLevel,
                s"Using cache for " +
                s"[${stringRepr}] at location " +
                s"[${roots.globalCacheRoot.toMaskedPath.applyMasking()}]."
              )
              return cache
            case None =>
          }

          // Load from the local root if the global root fails.
          loadCacheFrom(roots.localCacheRoot, context) match {
            case cache @ Some(_) =>
              logger.log(
                logLevel,
                s"Using cache for " +
                s"[${stringRepr}] at location " +
                s"[${roots.localCacheRoot.toMaskedPath.applyMasking()}]."
              )
              return cache
            case None =>
          }

          logger.log(
            logLevel,
            s"Unable to load a cache for module [${stringRepr}]"
          )

          None
        case None => None
      }
    }
  }

  /** Loads the cache from the provided `cacheRoot`, invalidating the cache if
    * the loading fails for any reason.
    *
    * @param cacheRoot the root at which to find the cache for this cache entry
    * @param logger    a logger
    * @return the cached data if available, otherwise [[None]].
    */
  private def loadCacheFrom(cacheRoot: TruffleFile, context: EnsoContext)(
    implicit logger: TruffleLogger
  ): Option[T] = {
    val metadataPath = getCacheMetadataPath(cacheRoot)
    val dataPath     = getCacheDataPath(cacheRoot)

    loadCacheMetadata(metadataPath) match {
      case Some(meta) =>
        val sourceDigestValid =
          computeDigestFromSource(context)
            .map(_ == meta.sourceHash)
            .getOrElse(false)
        val blobBytes       = dataPath.readAllBytes()
        val blobDigestValid = computeDigestFromBytes(blobBytes) == meta.blobHash

        if (sourceDigestValid && blobDigestValid) {
          val readObject =
            Using(new ObjectInputStream(new ByteArrayInputStream(blobBytes))) {
              _.readObject()
            }

          readObject match {
            case Success(obj) =>
              validateReadObject(obj, meta) match {
                case Failure(_) =>
                  logger.log(
                    logLevel,
                    s"${stringRepr}` was corrupt on disk."
                  )
                  invalidateCache(cacheRoot)
                  None
                case success =>
                  success.toOption
              }
            case Failure(ex) =>
              logger.log(
                logLevel,
                s"`${stringRepr}` failed to load " +
                s"(caused by: ${ex.getMessage})."
              )
              invalidateCache(cacheRoot)
              None
          }
        } else {
          logger.log(
            logLevel,
            s"One or more digests did not match for the cache for " +
            s"[${stringRepr}]."
          )
          invalidateCache(cacheRoot)
          None
        }
      case None =>
        logger.log(
          logLevel,
          s"Could not load the cache metadata " +
          s"at [${metadataPath.toMaskedPath.applyMasking()}]"
        )
        invalidateCache(cacheRoot)
        None
    }
  }

  /** Validates the deserialized data by returning the expected cached entry, or [[Failure]].
    *
    * @param obj deserialized object
    * @param meta metadata corresponding to the `obj`
    * @return an `obj` transformed to a cached entry, or [[Failure]]
    */
  protected def validateReadObject(obj: AnyRef, meta: M): Try[T]

  /** Read metadata representation from the provided location
    * @param path location of the serialized metadata
    * @return deserialized metadata, or [[None]] if invalid
    */
  private def loadCacheMetadata(
    path: TruffleFile
  ): Option[M] = {
    if (path.isReadable) {
      val bytes = path.readAllBytes()
      metadataFromBytes(bytes)
    } else {
      None
    }
  }

  protected def metadataFromBytes(bytes: Array[Byte]): Option[M]

  private def ensureRoot(cacheRoot: TruffleFile): Boolean = {
    try {
      if (cacheRoot.exists() && cacheRoot.isDirectory()) {
        return cacheRoot.isWritable
      } else {
        cacheRoot.createDirectories()
        return cacheRoot.isWritable
      }
    } catch {
      case _: IOException | _: UnsupportedOperationException |
          _: SecurityException | _: FileAlreadyExistsException =>
    }
    false
  }

  /** Computes SHA-224 digest from the provided bytes.
    */
  protected def computeDigestFromBytes(bytes: Array[Byte]): String = {
    Hex.toHexString(messageDigest().digest(bytes))
  }

  /** Raw (byte)  representation of the [[Metadata]].
    *
    * @param sourceDigest SHA-256 digest of source data
    * @param blobDigest SHA-256 digest of serialized data
    * @param entry cache entry
    * @return serialized metadata
    */
  protected def metadata(
    sourceDigest: String,
    blobDigest: String,
    entry: T
  ): Array[Byte]

  /** Gets the path to the cache data within the `cacheRoot`.
    *
    * @param cacheRoot the root of the cache for this entry
    * @return the name of the data file for this entry's cache
    */
  private def getCacheDataPath(cacheRoot: TruffleFile): TruffleFile = {
    cacheRoot.resolve(cacheFileName(dataSuffix))
  }

  private def getCacheMetadataPath(cacheRoot: TruffleFile): TruffleFile =
    cacheRoot.resolve(cacheFileName(metadataSuffix))

  /** Computes the cache file name for a given extension.
    *
    * @param suffix the extension
    * @return the cache file name with the provided `ext`
    */
  private def cacheFileName(suffix: String): String = {
    s"$entryName$suffix"
  }

  protected def stringRepr: String

  protected def entryName: String

  protected def dataSuffix: String

  protected def metadataSuffix: String

  protected def messageDigest(): MessageDigest = {
    new SHA3.Digest224()
  }

  /** Deletes the cache for this data in the provided `cacheRoot`.
    *
    * @param cacheRoot the root of the cache to delete
    * @param logger    a logger
    */
  private def invalidateCache(
    cacheRoot: TruffleFile
  )(implicit logger: TruffleLogger): Unit = {
    val metadataFile = getCacheMetadataPath(cacheRoot)
    val dataFile     = getCacheDataPath(cacheRoot)

    def doDeleteAt(file: TruffleFile): Unit = {
      try {
        if (file.exists()) {
          if (file.isWritable) {
            file.delete()
            logger.log(
              logLevel,
              s"Invalidated the cache at [${file.toMaskedPath.applyMasking()}]."
            )
          } else {
            logger.log(
              logLevel,
              s"Cannot invalidate the cache at " +
              s"[${file.toMaskedPath.applyMasking()}]. " +
              s"Cache location not writable."
            )
          }
        }
      } catch {
        case _: NoSuchFileException =>
        // If it doesn't exist, our work has already been done for us!
        case _: java.nio.file.DirectoryNotEmptyException | _: IOException |
            _: SecurityException =>
          logger.log(
            logLevel,
            s"Unable to delete the cache at " +
            s"[${cacheRoot.toMaskedPath.applyMasking()}]."
          )
      }
    }

    doDeleteAt(metadataFile)
    doDeleteAt(dataFile)
  }

  /** Invalidates all caches associated with this cache.
    *
    * @param context the langage context in which loading is taking place
    */
  def invalidate(context: EnsoContext): Unit = {
    this.synchronized {
      implicit val logger: TruffleLogger = context.getLogger(this.getClass)
      getCacheRoots(context).foreach { roots =>
        invalidateCache(roots.globalCacheRoot)
        invalidateCache(roots.localCacheRoot)
      }
    }
  }

}

object Cache {

  /** A representation of the cache roots. */
  case class Roots(localCacheRoot: TruffleFile, globalCacheRoot: TruffleFile)

  /** Write the provided `bytes` to the provided `file`.
    *
    * @param file  the file to write the bytes to
    * @param bytes the bytes to write into `file`
    * @return `true` if writing completed successfully, `false` otherwise
    */
  private def writeBytesTo(
    file: TruffleFile,
    bytes: Array[Byte]
  ): Boolean = {
    try {
      Using.resource(
        file.newOutputStream(
          StandardOpenOption.WRITE,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING
        )
      ) { stream =>
        stream.write(bytes)
      }

      true
    } catch {
      case _: IOException | _: SecurityException => false
    }
  }

  /** Internal storage for the cache metadata to enable easy way to verify de-serialisation.
    *
    * @param sourceHash       the hex-encoded SHA3-224 hash of the source data for
    *                         which the cache was created
    * @param blobHash         the hex-encoded SHA3-224 hash of the serialized blob as it was
    *                         written to disk
    */
  trait Metadata {
    def sourceHash: String
    def blobHash:   String
  }

  /** Convert a [[TruffleFile]] to a [[MaskedPath]] for the purposes of logging.
    *
    * @param truffleFile the file to convert.
    */
  implicit private class ToMaskedPath(truffleFile: TruffleFile) {
    def toMaskedPath: MaskedPath = {
      MaskedPath(Path.of(truffleFile.getPath))
    }
  }

}
