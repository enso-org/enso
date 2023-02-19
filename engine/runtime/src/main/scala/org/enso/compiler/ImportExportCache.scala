package org.enso.compiler

import org.enso.pkg.{Package, QualifiedName}
import buildinfo.Info
import com.oracle.truffle.api.{TruffleFile, TruffleLogger}
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._
import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex
import org.enso.compiler.data.BindingsMap
import org.enso.editions.LibraryName
import org.enso.interpreter.runtime.EnsoContext
import org.enso.logger.masking.MaskedPath

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  IOException,
  ObjectInputStream,
  ObjectOutputStream
}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{
  FileAlreadyExistsException,
  NoSuchFileException,
  Path,
  StandardOpenOption
}
import java.util.logging.Level
import scala.util.{Failure, Success, Using}

class ImportExportCache(library: LibraryName) {
  import ImportExportCache._

  def save(
    cache: Cache,
    context: EnsoContext,
    useGlobalCacheLocation: Boolean
  ): Option[TruffleFile] = this.synchronized {
    val logger: TruffleLogger = context.getLogger(this.getClass)
    getBindingsCacheRoots(context).flatMap(r =>
      context.getPackageRepository.getPackageForLibrary(library).map((r, _))
    ) match {
      case Some((roots, pkg)) =>
        if (useGlobalCacheLocation) {
          if (saveCacheTo(pkg, roots.globalCacheRoot, cache)(logger)) {
            return Some(roots.globalCacheRoot)
          }
        } else {
          logger.log(
            logLevel,
            s"Skipping use of global cache locations for module " +
            s"${library}."
          )
        }

        if (saveCacheTo(pkg, roots.localCacheRoot, cache)(logger)) {
          return Some(roots.localCacheRoot)
        }

        logger.log(
          logLevel,
          s"Unable to write cache data for module [${library}]."
        )
        None
      case None => None
    }
  }

  private def saveCacheTo(
    pkg: Package[TruffleFile],
    cacheRoot: TruffleFile,
    cache: Cache
  )(implicit logger: TruffleLogger): Boolean = {
    if (ensureRoot(cacheRoot)) {
      val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val bytesToWrite =
        Using.resource(new ObjectOutputStream(byteStream)) { stream =>
          stream.writeObject(cache)
          byteStream.toByteArray
        }

      val blobDigest = computeDigestOfBytes(bytesToWrite)
      val pkgDigest  = computePkgDigest(pkg)
      val metadata   = Metadata(pkgDigest, blobDigest)
      val metadataBytes =
        metadata.asJson.toString().getBytes(metadataCharset)

      val cacheDataFile = getCacheDataPath(cacheRoot)
      val metadataFile  = getCacheMetadataPath(cacheRoot)
      val parentPath    = cacheDataFile.getParent

      if (writeBytesTo(cacheDataFile, bytesToWrite)) {
        if (writeBytesTo(metadataFile, metadataBytes)) {
          logger.log(
            Level.INFO,
            s"Written bindings cache data for module " +
            s"[${library}] " +
            s"to [${parentPath}]."
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

  def load(context: EnsoContext): Option[Cache] =
    this.synchronized {
      implicit val logger: TruffleLogger = context.getLogger(this.getClass)
      logger.log(logLevel, "loading from Cache")
      getBindingsCacheRoots(context).flatMap(r =>
        context.getPackageRepository.getPackageForLibrary(library).map((r, _))
      ) match {
        case Some((roots, pkg)) =>
          logger.log(logLevel, "got roots: " + roots)
          // Load from the global root as a priority.
          loadCacheFrom(pkg, roots.globalCacheRoot) match {
            case cache @ Some(_) =>
              logger.log(
                logLevel,
                s"Using cache for library " +
                s"[${library}] at location " +
                s"[${roots.globalCacheRoot.toMaskedPath.applyMasking()}]."
              )
              return cache
            case None =>
          }

          // Load from the local root if the global root fails.
          loadCacheFrom(pkg, roots.localCacheRoot) match {
            case cache @ Some(_) =>
              logger.log(
                logLevel,
                s"Using cache for library " +
                s"[${library}] at location " +
                s"[${roots.localCacheRoot.toMaskedPath.applyMasking()}]."
              )
              return cache
            case None =>
          }

          logger.log(
            logLevel,
            s"Unable to load a cache for module [${library}]"
          )

          None
        case None => None
      }
    }

  private def loadCacheFrom(pkg: Package[TruffleFile], cacheRoot: TruffleFile)(
    implicit logger: TruffleLogger
  ): Option[Cache] = {
    val metadataPath = getCacheMetadataPath(cacheRoot)
    val dataPath     = getCacheDataPath(cacheRoot)

    loadCacheMetadata(metadataPath) match {
      case Some(meta) =>
        val sourceDigestValid =
          computePkgDigest(pkg) == meta.sourceHash
        val blobBytes       = dataPath.readAllBytes()
        val blobDigestValid = computeDigestOfBytes(blobBytes) == meta.blobHash

        if (sourceDigestValid && blobDigestValid) {
          val readObject =
            Using(new ObjectInputStream(new ByteArrayInputStream(blobBytes))) {
              _.readObject()
            }

          readObject match {
            case Success(cache: Cache) =>
              Some(cache)
            case Success(other) =>
              logger.log(
                logLevel,
                s"Module `${library}` was corrupt on disk." + other.getClass
              )
              None
            case Failure(ex) =>
              logger.log(
                logLevel,
                s"Module `${library}` failed to load " +
                s"(caused by: ${ex.getMessage})."
              )
              None
          }
        } else {
          logger.log(
            logLevel,
            s"One or more digests did not match for the cache for " +
            s"module [${library}]."
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

  private def loadCacheMetadata(
    path: TruffleFile
  ): Option[Metadata] = {
    if (path.isReadable) {
      val bytes           = path.readAllBytes()
      val maybeJsonString = new String(bytes, metadataCharset)

      val decoded =
        decode[Metadata](maybeJsonString).toOption

      decoded
    } else {
      None
    }
  }

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

  private def computePkgDigest(pkg: Package[TruffleFile]): String = {
    val digest = new SHA3.Digest224()
    pkg.listSources.sortBy(_.qualifiedName)(NameOrdering).foreach { source =>
      digest.update(source.file.readAllBytes())
    }
    Hex.toHexString(digest.digest())
  }

  private def computeDigestOfBytes(bytes: Array[Byte]): String = {
    Hex.toHexString(new SHA3.Digest224().digest(bytes))
  }

  private def getBindingsCacheRoots(
    context: EnsoContext
  ): Option[Roots] = {
    context.getPackageRepository.getPackageForLibrary(library).map { pkg =>
      val bindingsCacheRoot =
        pkg.getBindingsCacheRootForPackage(Info.ensoVersion)
      val localCacheRoot = bindingsCacheRoot.resolve(library.namespace)
      val distribution   = context.getDistributionManager
      val pathSegments = List(
        pkg.namespace,
        pkg.name,
        pkg.config.version,
        Info.ensoVersion,
        library.namespace
      )
      val path = distribution.LocallyInstalledDirectories.irCacheDirectory
        .resolve(pathSegments.mkString("/"))
      val globalCacheRoot = context.getTruffleFile(path.toFile)

      Roots(localCacheRoot, globalCacheRoot)
    }
  }

  // paths
  private def bindingsCacheDataName: String = {
    cacheFileName(bindingsCacheDataExtension)
  }

  /** @return the name of the metadata file for the cache
    */
  private def bindingsCacheMetadataName: String = {
    cacheFileName(bindingsCacheMetadataExtension)
  }

  /** Computes the cache file name for a given extension.
    *
    * @param ext the extension
    * @return the cache file name with the provided `ext`
    */
  private def cacheFileName(ext: String): String = {
    s"${library.name}$ext"
  }

  private def getCacheMetadataPath(cacheRoot: TruffleFile): TruffleFile =
    cacheRoot.resolve(bindingsCacheMetadataName)

  /** Gets the path to the cache data within the `cacheRoot`.
    *
    * @param cacheRoot the root of the cache for this module
    * @return the name of the data file for this module's cache
    */
  private def getCacheDataPath(cacheRoot: TruffleFile): TruffleFile = {
    cacheRoot.resolve(bindingsCacheDataName)
  }

}

object ImportExportCache {
  private val logLevel                               = Level.FINEST
  private val bindingsCacheDataExtension: String     = ".bindings"
  private val bindingsCacheMetadataExtension: String = ".bindings.meta"
  private val metadataCharset: Charset               = StandardCharsets.UTF_8

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

  @JsonCodec
  case class Metadata(
    sourceHash: String,
    blobHash: String
  )
  case class Cache(entries: Map[QualifiedName, BindingsMap])
  case class Roots(localCacheRoot: TruffleFile, globalCacheRoot: TruffleFile)

  private object NameOrdering extends Ordering[QualifiedName] {
    override def compare(x: QualifiedName, y: QualifiedName): Int = {
      x.toString.compare(y.toString)
    }
  }

  implicit private class ToMaskedPath(truffleFile: TruffleFile) {
    def toMaskedPath: MaskedPath = {
      MaskedPath(Path.of(truffleFile.getPath))
    }
  }
}
