package org.enso.compiler;

import buildinfo.Info;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import org.enso.compiler.data.BindingsMap;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.QualifiedName;
import org.enso.pkg.SourceFile;
import scala.collection.immutable.Map;
import scala.jdk.CollectionConverters;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;

public final class ImportExportCache extends Cache<ImportExportCache.CachedBindings, ImportExportCache.Metadata> {

    private final LibraryName libraryName;

    public ImportExportCache(LibraryName libraryName) {
        this.libraryName = libraryName;
        this.logLevel = Level.FINEST;
        this.stringRepr = libraryName.toString();
        this.entryName = libraryName.name();
        this.dataSuffix = bindingsCacheDataExtension;
        this.metadataSuffix = bindingsCacheMetadataExtension;
    }

    @Override
    protected byte[] metadata(String sourceDigest, String blobDigest, CachedBindings entry) {
        var mapper = new ObjectMapper();
        try {
            return mapper.writeValueAsString(new Metadata(sourceDigest, blobDigest)).getBytes(metadataCharset);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected CachedBindings validateReadObject(Object obj, Metadata meta, TruffleLogger logger) throws CacheException {
        if (obj instanceof MapToBindings bindings) {
            return new CachedBindings(libraryName, bindings, Optional.empty());
        } else {
            throw new CacheException("Expected ImportExportCache.FileToBindings, got " + obj.getClass());
        }
    }

    @Override
    protected Optional<Metadata> metadataFromBytes(byte[] bytes) {
        var maybeJsonString = new String(bytes, Cache.metadataCharset);
        var mapper = new ObjectMapper();
        try {
            return Optional.of(mapper.readValue(maybeJsonString, ImportExportCache.Metadata.class));
        } catch (JsonProcessingException e) {
            return Optional.empty();
        }
    }

    @Override
    protected Optional<String> computeDigest(CachedBindings entry, TruffleLogger logger) {
        return entry.sources().map(sources -> computeDigestOfLibrarySources(sources, logger));
    }
    @Override
    @SuppressWarnings("unchecked")
    protected Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
        return context
                .getPackageRepository()
                .getPackageForLibraryJava(libraryName)
                .map(pkg -> computeDigestOfLibrarySources(pkg.listSourcesJava(), logger));
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
        return context.getPackageRepository().getPackageForLibraryJava(libraryName).map(pkg -> {
            var bindingsCacheRoot =
                    pkg.getBindingsCacheRootForPackage(Info.ensoVersion());
            var localCacheRoot = bindingsCacheRoot.resolve(libraryName.namespace());
            var distribution = context.getDistributionManager();
            var pathSegments = CollectionConverters.ListHasAsScala(Arrays.asList(
                    pkg.namespace(),
                    pkg.name(),
                    pkg.config().version(),
                    Info.ensoVersion(),
                    libraryName.namespace()
            )).asScala();
            var path = distribution.LocallyInstalledDirectories().irCacheDirectory()
                    .resolve(pathSegments.mkString("/"));
            var globalCacheRoot = context.getTruffleFile(path.toFile());
            return new Cache.Roots(localCacheRoot, globalCacheRoot);
        });
    }

    @Override
    protected Object extractObjectToSerialize(CachedBindings entry) {
        return entry.bindings();
    }

    static class MapToBindings implements Serializable {
        private final Map<QualifiedName, BindingsMap> _entries;

        public MapToBindings(Map<QualifiedName, BindingsMap> entries) {
            this._entries = entries;
        }

        Map<QualifiedName, BindingsMap> entries() {
            return _entries;
        }
    }

    // CachedBindings is not a record **on purpose**. There appears to be a Frgaal bug leading to invalid compilation error.
    static class CachedBindings {
        private final LibraryName _libraryName;
        private final MapToBindings _bindings;
        private final Optional<List<SourceFile<TruffleFile>>> _sources;
        public CachedBindings(LibraryName libraryName, MapToBindings bindings, Optional<List<SourceFile<TruffleFile>>> sources) {
            this._libraryName = libraryName;
            this._bindings = bindings;
            this._sources = sources;
        }

        LibraryName libraryName() {
            return _libraryName;
        }
        MapToBindings bindings() {
            return _bindings;
        }
        Optional<List<SourceFile<TruffleFile>>> sources() {
            return _sources;
        }

    }

    public record Metadata(
            @JsonProperty("source_hash") String sourceHash,
            @JsonProperty("blob_hash") String blobHash) implements Cache.Metadata {}

    private static final String bindingsCacheDataExtension = ".bindings";
    private static final String bindingsCacheMetadataExtension =".bindings.meta";

}
