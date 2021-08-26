package org.enso.compiler

import buildinfo.Info
import com.oracle.truffle.api.{TruffleFile, TruffleLogger}
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._
import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex
import org.enso.compiler.ModuleCache.ToMaskedPath
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.{Context, Module}
import org.enso.logger.masking.MaskedPath

import java.io.IOException
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file._
import java.util.logging.Level
import scala.jdk.OptionConverters._
import scala.util.Using

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
    * @return returns the location of the cache if successful, and [[None]] if
    *         it was unable to save
    */
  def save(
    module: ModuleCache.CachedModule,
    context: Context
  ): Option[TruffleFile] = {
    implicit val logger: TruffleLogger = context.getLogger(this.getClass)
    val roots                          = getIrCacheRoots(context)

    roots.globalCacheRoot match {
      case Some(root) =>
        if (saveCacheTo(root, module)) {
          return Some(root)
        }
      case None =>
    }

    roots.localCacheRoot match {
      case Some(root) =>
        if (saveCacheTo(root, module)) {
          return Some(root)
        }
      case None =>
    }

    logger.log(
      Level.INFO,
      s"Unable to write cache data for module [${this.module.getName.toString}]."
    )

    None
  }

  /** Loads the cache for this module where possible.
    *
    * @param context the langage context in which loading is taking place
    * @return the cached module data if possible, and [[None]] if it could not
    *         load a valid cache
    */
  //noinspection DuplicatedCode
  def load(context: Context): Option[ModuleCache.CachedModule] = {
    implicit val logger: TruffleLogger = context.getLogger(this.getClass)
    val roots                          = getIrCacheRoots(context)

    // Load from the global root as a priority.
    roots.globalCacheRoot match {
      case Some(root) =>
        loadCacheFrom(root) match {
          case cache @ Some(_) =>
            logger.log(
              Level.INFO,
              s"Using cache for module " +
              s"[${module.getName.toString}] at location " +
              s"[${root.toMaskedPath.applyMasking()}]."
            )
            return cache
          case None =>
        }
      case None =>
    }

    // Load from the local root if the global root fails.
    roots.localCacheRoot match {
      case Some(root) =>
        loadCacheFrom(root) match {
          case cache @ Some(_) =>
            logger.log(
              Level.INFO,
              s"Using cache for module " +
              s"[${module.getName.toString}] at location " +
              s"[${root.toMaskedPath.applyMasking()}]."
            )
            return cache
          case None =>
        }
      case None =>
    }

    logger.log(
      Level.INFO,
      s"Unable to load a cache for module [${module.getName.toString}]"
    )

    None
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
      // TODO [AA] Use dummy data for the moment.
      val bytesToWrite = Array[Byte](0, 1, 2, 3)

      val blobDigest       = computeDigest(bytesToWrite)
      val sourceDigest     = computeSourceDigest()
      val compilationStage = module.compilationStage.toString
      val metadata =
        ModuleCache.Metadata(sourceDigest, blobDigest, compilationStage)
      val metadataBytes =
        metadata.asJson.toString().getBytes(ModuleCache.metadataCharset)

      val cacheDataFile = getCacheDataPath(cacheRoot)
      val metadataFile  = getCacheMetadataPath(cacheRoot)

      if (writeBytesTo(cacheDataFile, bytesToWrite)) {
        logger.log(
          Level.INFO,
          s"Written cache data for module " +
          s"[${this.module.getName.toString}] " +
          s"to [${cacheDataFile.toMaskedPath.applyMasking()}]."
        )

        if (writeBytesTo(metadataFile, metadataBytes)) {
          return true
        } else {
          // Clean up after ourselves if it fails.
          cacheDataFile.delete()
        }
      }
    }

    false
  }

  /** Write the provided `bytes` to the provided `file`.
    *
    * @param file the file to write the bytes to
    * @param bytes the bytes to write into `file`
    * @return `true` if writing completed successfully, `false` otherwise
    */
  private def writeBytesTo(file: TruffleFile, bytes: Array[Byte]): Boolean = {
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
        val sourceDigestValid = computeSourceDigest() == meta.sourceHash
        val blobDigestValid   = computeBlobDigest(dataPath) == meta.blobHash
        val compilationStage =
          Module.CompilationStage.valueOf(meta.compilationStage)

        if (sourceDigestValid && blobDigestValid) {
          Some(
            ModuleCache.CachedModule(
              // TODO [AA] This is a dummy as we don't write the cache yet.
              IR.Module(List(), List(), List(), None),
              compilationStage
            )
          )
        } else {
          logger.log(
            Level.INFO,
            s"One or more digests did not match for the cache for " +
            s"module [${module.getName.toString}]."
          )
          invalidateCache(cacheRoot)
          None
        }
      case None =>
        logger.log(
          Level.INFO,
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
        if (file.isWritable) {
          file.delete()
          logger.log(
            Level.INFO,
            s"Invalidated the cache at [${file.toMaskedPath.applyMasking()}]."
          )
        } else {
          logger.log(
            Level.INFO,
            s"Cannot invalidate the cache at " +
            s"[${file.toMaskedPath.applyMasking()}]. " +
            s"Cache location not writable."
          )
        }
      } catch {
        case _: NoSuchFileException =>
        // If it doesn't exist, our work has already been done for us!
        case _: DirectoryNotEmptyException | _: IOException |
            _: SecurityException =>
          logger.log(
            Level.SEVERE,
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
  private def getIrCacheRoots(context: Context): ModuleCache.Roots = {
    val roots = new ModuleCache.Roots

    context.getPackageOf(module.getSourceFile).toScala match {
      case Some(pkg) =>
        val irCacheRoot = pkg.getIrCacheRootForPackage(Info.ensoVersion)
        val moduleName  = context.getModuleNameForFile(irCacheRoot).toScala
        roots.localCacheRoot = moduleName.map(qualName =>
          irCacheRoot.resolve(qualName.path.mkString("/"))
        )

        moduleName.foreach(qualName => {
          roots.localCacheRoot =
            Some(irCacheRoot.resolve(qualName.path.mkString("/")))

          val distribution = context.getDistributionManager
          val pathSegments =
            List(pkg.namespace, pkg.name, pkg.config.version) ++ qualName.path

          val path =
            distribution.LocallyInstalledDirectories.irCacheDirectory.resolve(
              pathSegments.mkString("/")
            )
          roots.globalCacheRoot = Some(context.getTruffleFile(path.toFile))
        })
      case None =>
    }

    roots
  }

  // === Utilities ============================================================

  /** Ensures that the provided cache root exists and is writable.
    *
    * @param cacheRoot the cache root
    * @return `true` if the root exists and is writable, `false` otherwise
    */
  private def ensureRoot(cacheRoot: TruffleFile): Boolean = {
    try {
      if (cacheRoot.isDirectory()) {
        cacheRoot.createDirectories()
        cacheRoot.isWritable
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
  private def computeSourceDigest(): String = {
    val sourceBytes = module.getSourceFile.readAllBytes()
    computeDigest(sourceBytes)
  }

  /** Computes the SHA3-224 digest of the provided file.
    *
    * @param file the file to compute the digest of
    * @return the hex string representing the digest of `file`
    */
  private def computeBlobDigest(file: TruffleFile): String = {
    val blobBytes = file.readAllBytes()
    computeDigest(blobBytes)
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

  /** A representation of the cache roots for the module. */
  class Roots() {
    var localCacheRoot: Option[TruffleFile]  = None
    var globalCacheRoot: Option[TruffleFile] = None
  }

  /** A representation of a module for the cache.
    *
    * @param module the module
    * @param compilationStage the compilation stage of the module at the point
    *                         at which it was provided for serialisation
    */
  case class CachedModule(
    module: IR.Module,
    compilationStage: Module.CompilationStage
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
