package org.enso.compiler

import java.io._
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file._
import java.util.logging.Level

import buildinfo.Info
import com.oracle.truffle.api.source.Source
import com.oracle.truffle.api.{TruffleFile, TruffleLogger}
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._
import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex
import org.enso.compiler.ModuleCache.{logLevel, ToMaskedPath}
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.{Context, Module}
import org.enso.logger.masking.MaskedPath

import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Using}

// TODO Once #1971 is fixed, the logging statements should go back to using our
//  normal templating syntax.

/** Responsible for the management of caches associated with a given module.
  *
  * @param module the module for which the caches are managed.
  */
class ModuleCache(private val module: Module) {

  // === Interface ============================================================

  /** Saves the provided IR into the cache for this module.
    *
    * @param module the module representation to be saved
    * @param context the language context in which saving is taking place
    * @param useGlobalCacheLocations
    * @return returns the location of the cache if successful, and [[None]] if
    *         it was unable to save
    */
  def save(
    module: ModuleCache.CachedModule,
    context: Context,
    useGlobalCacheLocations: Boolean
  ): Option[TruffleFile] = this.synchronized {
    implicit val logger: TruffleLogger = context.getLogger(this.getClass)
    getIrCacheRoots(context) match {
      case Some(roots) =>
        if (useGlobalCacheLocations) {
          if (saveCacheTo(roots.globalCacheRoot, module)) {
            return Some(roots.globalCacheRoot)
          }
        } else {
          logger.log(
            logLevel,
            s"Skipping use of global cache locations for module " +
            s"${this.module.getName}."
          )
        }

        if (saveCacheTo(roots.localCacheRoot, module)) {
          return Some(roots.localCacheRoot)
        }

        logger.log(
          ModuleCache.logLevel,
          s"Unable to write cache data for module [${this.module.getName.toString}]."
        )
        None
      case None => None
    }
  }

  /** Loads the cache for this module where possible.
    *
    * @param context the langage context in which loading is taking place
    * @return the cached module data if possible, and [[None]] if it could not
    *         load a valid cache
    */
  //noinspection DuplicatedCode
  def load(context: Context): Option[ModuleCache.CachedModule] =
    this.synchronized {
      implicit val logger: TruffleLogger = context.getLogger(this.getClass)
      getIrCacheRoots(context) match {
        case Some(roots) =>
          // Load from the global root as a priority.
          loadCacheFrom(roots.globalCacheRoot) match {
            case cache @ Some(_) =>
              logger.log(
                ModuleCache.logLevel,
                s"Using cache for module " +
                s"[${module.getName.toString}] at location " +
                s"[${roots.globalCacheRoot.toMaskedPath.applyMasking()}]."
              )
              return cache
            case None =>
          }

          // Load from the local root if the global root fails.
          loadCacheFrom(roots.localCacheRoot) match {
            case cache @ Some(_) =>
              logger.log(
                ModuleCache.logLevel,
                s"Using cache for module " +
                s"[${module.getName.toString}] at location " +
                s"[${roots.localCacheRoot.toMaskedPath.applyMasking()}]."
              )
              return cache
            case None =>
          }

          logger.log(
            ModuleCache.logLevel,
            s"Unable to load a cache for module [${module.getName.toString}]"
          )

          None
        case None => None
      }
    }

  /** Invalidates all caches associated with the module.
    *
    * @param context the langage context in which loading is taking place
    */
  def invalidate(context: Context): Unit = {
    this.synchronized {
      implicit val logger: TruffleLogger = context.getLogger(this.getClass)
      getIrCacheRoots(context).foreach { roots =>
        invalidateCache(roots.globalCacheRoot)
        invalidateCache(roots.localCacheRoot)
      }
    }
  }

  // === Internals ============================================================

  /** Saves the cache into the provided `cacheRoot`.
    *
    * @param cacheRoot the root into which the cache should be saved
    * @param module the data that should be cached for this module
    * @param logger a logger
    * @return `true` if the cache was written successfully, `false` otherwise
    */
  private def saveCacheTo(
    cacheRoot: TruffleFile,
    module: ModuleCache.CachedModule
  )(implicit logger: TruffleLogger): Boolean = {
    if (ensureRoot(cacheRoot)) {
      val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val bytesToWrite =
        Using.resource(new ObjectOutputStream(byteStream)) { stream =>
          stream.writeObject(module.module)
          byteStream.toByteArray
        }

      val blobDigest       = computeDigest(bytesToWrite)
      val sourceDigest     = computeSourceDigest(module.source)
      val compilationStage = module.compilationStage.toString
      val metadata =
        ModuleCache.Metadata(sourceDigest, blobDigest, compilationStage)
      val metadataBytes =
        metadata.asJson.toString().getBytes(ModuleCache.metadataCharset)

      val cacheDataFile = getCacheDataPath(cacheRoot)
      val metadataFile  = getCacheMetadataPath(cacheRoot)
      val parentPath    = cacheDataFile.getParent

      if (writeBytesTo(cacheDataFile, bytesToWrite)) {
        if (writeBytesTo(metadataFile, metadataBytes)) {
          logger.log(
            ModuleCache.logLevel,
            s"Written cache data for module " +
            s"[${this.module.getName.toString}] " +
            s"to [${parentPath.toMaskedPath.applyMasking()}]."
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

  /** Write the provided `bytes` to the provided `file`.
    *
    * @param file the file to write the bytes to
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

  /** Loads the cache from the provided `cacheRoot`, invalidating the cache if
    * the loading fails for any reason.
    *
    * @param cacheRoot the root at which to find the cache for this module
    * @param logger a logger
    * @return the cached module representation if available, otherwise [[None]].
    */
  private def loadCacheFrom(
    cacheRoot: TruffleFile
  )(implicit logger: TruffleLogger): Option[ModuleCache.CachedModule] = {
    val metadataPath = getCacheMetadataPath(cacheRoot)
    val dataPath     = getCacheDataPath(cacheRoot)

    loadCacheMetadata(metadataPath) match {
      case Some(meta) =>
        val sourceDigestValid =
          computeSourceDigest(module.getSource) == meta.sourceHash
        val blobBytes       = dataPath.readAllBytes()
        val blobDigestValid = computeDigest(blobBytes) == meta.blobHash
        val compilationStage =
          Module.CompilationStage.valueOf(meta.compilationStage)

        if (sourceDigestValid && blobDigestValid) {
          val readObject =
            Using(new ObjectInputStream(new ByteArrayInputStream(blobBytes))) {
              _.readObject()
            }

          readObject match {
            case Success(mod: IR.Module) =>
              Some(
                ModuleCache.CachedModule(
                  mod,
                  compilationStage,
                  module.getSource
                )
              )
            case Success(_) =>
              logger.log(
                ModuleCache.logLevel,
                s"Module `${module.getName.toString}` was corrupt on disk."
              )
              None
            case Failure(ex) =>
              logger.log(
                ModuleCache.logLevel,
                s"Module `${module.getName.toString}` failed to load " +
                s"(caused by: ${ex.getMessage})."
              )
              None
          }
        } else {
          logger.log(
            ModuleCache.logLevel,
            s"One or more digests did not match for the cache for " +
            s"module [${module.getName.toString}]."
          )
          invalidateCache(cacheRoot)
          None
        }
      case None =>
        logger.log(
          ModuleCache.logLevel,
          s"Could not load the cache metadata " +
          s"at [${metadataPath.toMaskedPath.applyMasking()}]"
        )
        invalidateCache(cacheRoot)
        None
    }
  }

  /** Loads the cache metadata into an object from the provided path.
    *
    * It provides _no_ verification of the provided path, and will
    * unconditionally try and load it.
    *
    * @param path the path to the metadata file
    * @return an object representation of the metadata at `path`, where possible
    */
  private def loadCacheMetadata(
    path: TruffleFile
  ): Option[ModuleCache.Metadata] = {
    if (path.isReadable) {
      val bytes           = path.readAllBytes()
      val maybeJsonString = new String(bytes, ModuleCache.metadataCharset)

      val decoded =
        decode[ModuleCache.Metadata](maybeJsonString).toOption

      decoded
    } else {
      None
    }
  }

  /** Deletes the cache for this module in the provided `cacheRoot`.
    *
    * @param cacheRoot the root of the cache to delete
    * @param logger a logger
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
              ModuleCache.logLevel,
              s"Invalidated the cache at [${file.toMaskedPath.applyMasking()}]."
            )
          } else {
            logger.log(
              ModuleCache.logLevel,
              s"Cannot invalidate the cache at " +
              s"[${file.toMaskedPath.applyMasking()}]. " +
              s"Cache location not writable."
            )
          }
        }
      } catch {
        case _: NoSuchFileException =>
        // If it doesn't exist, our work has already been done for us!
        case _: DirectoryNotEmptyException | _: IOException |
            _: SecurityException =>
          logger.log(
            ModuleCache.logLevel,
            s"Unable to delete the cache at " +
            s"[${cacheRoot.toMaskedPath.applyMasking()}]."
          )
      }
    }

    doDeleteAt(metadataFile)
    doDeleteAt(dataFile)
  }

  /** Gets the potential cache roots for this module.
    *
    * @param context the language context in which the cache is being run
    * @return a representation of the local and global cache roots for this
    *         module
    */
  private def getIrCacheRoots(context: Context): Option[ModuleCache.Roots] = {
    if (module != context.getBuiltins.getModule) {
      context.getPackageOf(module.getSourceFile).toScala.map { pkg =>
        val irCacheRoot    = pkg.getIrCacheRootForPackage(Info.ensoVersion)
        val qualName       = module.getName
        val localCacheRoot = irCacheRoot.resolve(qualName.path.mkString("/"))

        val distribution = context.getDistributionManager
        val pathSegments = List(
          pkg.namespace,
          pkg.name,
          pkg.config.version,
          Info.ensoVersion
        ) ++ qualName.path
        val path = distribution.LocallyInstalledDirectories.irCacheDirectory
          .resolve(pathSegments.mkString("/"))
        val globalCacheRoot = context.getTruffleFile(path.toFile)

        ModuleCache.Roots(localCacheRoot, globalCacheRoot)
      }
    } else {
      val distribution = context.getDistributionManager
      val pathSegments = List(
        Builtins.NAMESPACE,
        Builtins.PACKAGE_NAME,
        Info.ensoVersion,
        Info.ensoVersion
      ) ++ module.getName.path
      val path = distribution.LocallyInstalledDirectories.irCacheDirectory
        .resolve(pathSegments.mkString("/"))
      val globalCacheRoot = context.getTruffleFile(path.toFile)

      Some(ModuleCache.Roots(globalCacheRoot, globalCacheRoot))
    }
  }

  // === Utilities ============================================================

  /** Ensures that the provided cache root exists and is writable.
    *
    * @param cacheRoot the cache root
    * @return `true` if the root exists and is writable, `false` otherwise
    */
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

  /** Computes the SHA3-224 digest of the source file that underlies this
    * module.
    *
    * @return the hex string representing the digest
    */
  private def computeSourceDigest(source: Source): String = {
    val sourceBytes = if (source.hasBytes) {
      source.getBytes.toByteArray
    } else {
      source.getCharacters.toString.getBytes(StandardCharsets.UTF_8)
    }

    computeDigest(sourceBytes)
  }

  /** Computes the SHA3-224 digest of the provided byte array.
    *
    * @param bytes the bytes to compute the digest of
    * @return the hex string representing the digest of `bytes`
    */
  private def computeDigest(bytes: Array[Byte]): String = {
    Hex.toHexString(new SHA3.Digest224().digest(bytes))
  }

  /** @return the name of the data file for the cache
    */
  private def irCacheDataName: String = {
    cacheFileName(ModuleCache.irCacheDataExtension)
  }

  /** @return the name of the metadata file for the cache
    */
  private def irCacheMetadataName: String = {
    cacheFileName(ModuleCache.irCacheMetadataExtension)
  }

  /** Computes the cache file name for a given extension.
    *
    * @param ext the extension
    * @return the cache file name with the provided `ext`
    */
  private def cacheFileName(ext: String): String = {
    s"${module.getName.item}$ext"
  }

  /** Gets the path to the cache metadata within the `cacheRoot`.
    *
    * @param cacheRoot the root of the cache for this module
    * @return the name of the metadata for this module's cache
    */
  private def getCacheMetadataPath(cacheRoot: TruffleFile): TruffleFile =
    cacheRoot.resolve(irCacheMetadataName)

  /** Gets the path to the cache data within the `cacheRoot`.
    *
    * @param cacheRoot the root of the cache for this module
    * @return the name of the data file for this module's cache
    */
  private def getCacheDataPath(cacheRoot: TruffleFile): TruffleFile = {
    cacheRoot.resolve(irCacheDataName)
  }
}
object ModuleCache {

  val irCacheDataExtension: String     = ".ir"
  val irCacheMetadataExtension: String = ".meta"

  val maximumBlockSizeBytes: Int = 1 * 1024 * 1024 * 10 // 10 MB
  val metadataCharset: Charset   = StandardCharsets.UTF_8

  /** The default logging level. */
  private val logLevel = Level.FINEST

  /** A representation of the cache roots for the module. */
  case class Roots(localCacheRoot: TruffleFile, globalCacheRoot: TruffleFile)

  /** A representation of a module for the cache.
    *
    * @param module the module
    * @param compilationStage the compilation stage of the module at the point
    *                         at which it was provided for serialisation
    * @param source the source of the module
    */
  case class CachedModule(
    module: IR.Module,
    compilationStage: Module.CompilationStage,
    source: Source
  )

  /** Internal storage for the cache metadata to enable easy serialisation and
    * deserialisation.
    *
    * @param sourceHash the hex-encoded SHA3-224 hash of the source file for
    *                   which the cache was created
    * @param blobHash the hex-encoded SHA3-224 hash of the IR blob as it was
    *                 written to disk
    * @param compilationStage the compilation stage at which the IR blob was
    *                         serialised
    */
  @JsonCodec
  case class Metadata(
    sourceHash: String,
    blobHash: String,
    compilationStage: String
  )

  /** Convert a [[TruffleFile]] to a [[MaskedPath]] for the purposes of logging.
    *
    * @param truffleFile the file to convert.
    */
  implicit class ToMaskedPath(truffleFile: TruffleFile) {
    def toMaskedPath: MaskedPath = {
      MaskedPath(Path.of(truffleFile.getPath))
    }
  }
}
