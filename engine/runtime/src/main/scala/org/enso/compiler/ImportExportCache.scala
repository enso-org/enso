package org.enso.compiler

import org.enso.pkg.{QualifiedName, SourceFile}
import buildinfo.Info
import com.oracle.truffle.api.TruffleFile
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._
import org.bouncycastle.util.encoders.Hex
import org.enso.compiler.Cache.Roots
import org.enso.compiler.data.BindingsMap
import org.enso.editions.LibraryName
import org.enso.interpreter.runtime.EnsoContext

import java.nio.charset.{Charset, StandardCharsets}
import java.util.logging.Level
import scala.util.{Failure, Success, Try}

class ImportExportCache(library: LibraryName)
    extends Cache[ImportExportCache.CacheBindings] {
  import ImportExportCache._

  override type M = Metadata

  override val logLevel: Level    = Level.FINEST
  override def stringRepr: String = library.toString

  override def computeDigestFromSource(
    context: EnsoContext
  ): Option[String] = {
    context.getPackageRepository.getPackageForLibrary(library).map { pkg =>
      computeDigestOfLibrarySources(pkg.listSources())
    }
  }

  override def computeDigest(
    entry: ImportExportCache.CacheBindings
  ): Option[String] = {
    entry.sources.map(computeDigestOfLibrarySources)
  }

  private def computeDigestOfLibrarySources(
    pkgSources: List[SourceFile[TruffleFile]]
  ): String = {
    val digest = messageDigest()
    pkgSources.sortBy(_.qualifiedName)(NameOrdering).foreach { source =>
      digest.update(source.file.readAllBytes())
    }
    Hex.toHexString(digest.digest())
  }

  override def getCacheRoots(context: EnsoContext): Option[Cache.Roots] = {
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

  override protected def extractObjectToSerialize(
    entry: CacheBindings
  ): AnyRef = {
    entry.bindings
  }

  override protected def validateReadObject(
    obj: AnyRef,
    meta: M
  ): Try[CacheBindings] =
    obj match {
      case bindings: MapToBindings =>
        Success(ImportExportCache.CacheBindings(library, bindings, None))
      case other =>
        Failure(
          new RuntimeException(
            s"unexpected deserialized object of ${other.getClass}"
          )
        )
    }

  override protected def metadataFromBytes(
    bytes: Array[Byte]
  ): Option[Metadata] = {
    val maybeJsonString = new String(bytes, metadataCharset)
    decode[Metadata](maybeJsonString).toOption
  }

  override protected def metadata(
    sourceDigest: String,
    blobDigest: String,
    entry: CacheBindings
  ): Array[Byte] = {
    val metadata = Metadata(sourceDigest, blobDigest)
    metadata.asJson.toString().getBytes(metadataCharset)
  }

  override protected def entryName: String = {
    library.name
  }

  override protected val dataSuffix: String = {
    bindingsCacheDataExtension
  }

  override protected def metadataSuffix: String = {
    bindingsCacheMetadataExtension
  }
}

object ImportExportCache {
  private val bindingsCacheDataExtension: String     = ".bindings"
  private val bindingsCacheMetadataExtension: String = ".bindings.meta"
  private val metadataCharset: Charset               = StandardCharsets.UTF_8

  @JsonCodec
  case class Metadata(
    sourceHash: String,
    blobHash: String
  ) extends Cache.Metadata

  case class CacheBindings(
    libraryName: LibraryName,
    bindings: MapToBindings,
    sources: Option[List[SourceFile[TruffleFile]]]
  )

  // Wrapper around map of bindings to avoid warnings about erasure
  // when matching the deserialized object
  case class MapToBindings(entries: Map[QualifiedName, BindingsMap])

  private object NameOrdering extends Ordering[QualifiedName] {
    override def compare(x: QualifiedName, y: QualifiedName): Int = {
      x.toString.compare(y.toString)
    }
  }

}
