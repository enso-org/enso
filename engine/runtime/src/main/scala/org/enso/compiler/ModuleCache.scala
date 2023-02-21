package org.enso.compiler

import java.nio.charset.{Charset, StandardCharsets}
import java.util.logging.Level
import buildinfo.Info
import com.oracle.truffle.api.source.Source
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._

import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.{EnsoContext, Module}

import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Try}

// TODO Once #1971 is fixed, the logging statements should go back to using our
//  normal templating syntax.

/** Responsible for the management of caches associated with a given module.
  *
  * @param module the module for which the caches are managed.
  */
class ModuleCache(private val module: Module)
    extends Cache[ModuleCache.CachedModule] {
  import org.enso.compiler.ModuleCache.{
    irCacheDataExtension,
    irCacheMetadataExtension,
    CachedModule
  }

  override type M = ModuleCache.Metadata

  override val logLevel: Level = Level.FINEST

  override def stringRepr: String = module.getName.toString

  override def computeDigestFromSource(
    context: EnsoContext
  ): Option[String] = {
    computeDigestOfModuleSources(module.getSource)
  }

  override def computeDigest(
    entry: ModuleCache.CachedModule
  ): Option[String] = {
    computeDigestOfModuleSources(entry.source)
  }

  private def computeDigestOfModuleSources(source: Source): Option[String] = {
    if (source != null) {
      val sourceBytes = if (source.hasBytes) {
        source.getBytes.toByteArray
      } else {
        source.getCharacters.toString.getBytes(StandardCharsets.UTF_8)
      }
      Some(computeDigestFromBytes(sourceBytes))
    } else None
  }

  override def getCacheRoots(context: EnsoContext): Option[Cache.Roots] = {
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

        Cache.Roots(localCacheRoot, globalCacheRoot)
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

      Some(Cache.Roots(globalCacheRoot, globalCacheRoot))
    }
  }

  override protected def extractObjectToSerialize(
    entry: CachedModule
  ): AnyRef = {
    entry.module
  }

  override protected def validateReadObject(
    obj: AnyRef,
    meta: M
  ): Try[CachedModule] = obj match {
    case ir: IR.Module =>
      Success(
        CachedModule(
          ir,
          Module.CompilationStage.valueOf(meta.compilationStage),
          module.getSource
        )
      )
    case other =>
      Failure(
        new RuntimeException(
          s"unexpected deserialized object of ${other.getClass}"
        )
      )
  }

  override protected def metadataFromBytes(bytes: Array[Byte]): Option[M] = {
    val maybeJsonString = new String(bytes, ModuleCache.metadataCharset)
    decode[ModuleCache.Metadata](maybeJsonString).toOption
  }

  override protected def metadata(
    sourceDigest: String,
    blobDigest: String,
    entry: CachedModule
  ): Array[Byte] = {
    val metadata = ModuleCache.Metadata(
      sourceDigest,
      blobDigest,
      entry.compilationStage.toString
    )
    metadata.asJson.toString().getBytes(ModuleCache.metadataCharset)
  }

  override protected def entryName: String = module.getName.item

  override protected def dataSuffix: String = irCacheDataExtension

  override protected def metadataSuffix: String = irCacheMetadataExtension
}
object ModuleCache {

  val irCacheDataExtension: String     = ".ir"
  val irCacheMetadataExtension: String = ".meta"
  val metadataCharset: Charset         = StandardCharsets.UTF_8

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
  ) extends Cache.Metadata

}
