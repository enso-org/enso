package org.enso.interpreter.caches;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;

import org.apache.commons.lang3.StringUtils;
import org.enso.compiler.core.Persistance;
import org.enso.compiler.data.BindingsMap;
import org.enso.editions.LibraryName;
import org.enso.interpreter.dsl.Persistable;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.QualifiedName;
import org.enso.pkg.SourceFile;
import org.openide.util.lookup.ServiceProvider;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;

import buildinfo.Info;
import scala.Tuple2;
import scala.collection.immutable.Map;

@Persistable(clazz = QualifiedName.class, id = 30300)
public final class ImportExportCache extends Cache<ImportExportCache.CachedBindings, ImportExportCache.Metadata> {

    private final LibraryName libraryName;

    public ImportExportCache(LibraryName libraryName) {
        super(Level.FINEST, libraryName.toString(), true, false);
        this.libraryName = libraryName;
        this.entryName = libraryName.name();
        this.dataSuffix = bindingsCacheDataExtension;
        this.metadataSuffix = bindingsCacheMetadataExtension;
    }

    @Override
    protected byte[] metadata(String sourceDigest, String blobDigest, CachedBindings entry) {
        try {
            return objectMapper.writeValueAsString(new Metadata(sourceDigest, blobDigest)).getBytes(metadataCharset);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected CachedBindings deserialize(EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger) throws ClassNotFoundException, IOException, ClassNotFoundException {
      var ref = Persistance.readObject(data);
      var bindings = ref.get(MapToBindings.class);
      return new CachedBindings(libraryName, bindings, Optional.empty());
    }

    @Override
    protected Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger) {
        var maybeJsonString = new String(bytes, Cache.metadataCharset);
        var mapper = new ObjectMapper();
        try {
            return Optional.of(objectMapper.readValue(maybeJsonString, ImportExportCache.Metadata.class));
        } catch (JsonProcessingException e) {
            logger.log(logLevel, "Failed to deserialize library's metadata.", e);
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
            var pathSegments = new String[]{
                    pkg.namespace(),
                    pkg.normalizedName(),
                    pkg.getConfig().version(),
                    Info.ensoVersion(),
                    libraryName.namespace()
            };
            var path = distribution.LocallyInstalledDirectories().irCacheDirectory()
                    .resolve(StringUtils.join(pathSegments, "/"));
            var globalCacheRoot = context.getTruffleFile(path.toFile());
            return new Cache.Roots(localCacheRoot, globalCacheRoot);
        });
    }

    @Override
    protected byte[] serialize(EnsoContext context, CachedBindings entry) throws IOException {
      var arr = Persistance.writeObject(entry.bindings());
      return arr;
    }

    public static final class MapToBindings  {
        private final Map<QualifiedName, BindingsMap> _entries;

        public MapToBindings(Map<QualifiedName, BindingsMap> entries) {
            this._entries = entries;
        }

        public Map<QualifiedName, BindingsMap> entries() {
            return _entries;
        }
    }

    @ServiceProvider(service = Persistance.class)
    public static final class PersistMapToBindings extends Persistance<MapToBindings> {
      public PersistMapToBindings() {
        super(MapToBindings.class, false, 364);
      }

      @Override
      protected void writeObject(MapToBindings obj, Output out) throws IOException {
        out.writeInt(obj.entries().size());
        var it = obj.entries().iterator();
        while (it.hasNext()) {
          var e = it.next();
          out.writeInline(QualifiedName.class, e._1());
          out.writeObject(e._2());
        }
      }

      @Override
      @SuppressWarnings("unchecked")
      protected MapToBindings readObject(Input in) throws IOException, ClassNotFoundException {
        var size = in.readInt();
        var b = Map.newBuilder();
        b.sizeHint(size);
        while (size-- > 0) {
          var name = in.readInline(QualifiedName.class);
          var value = in.readObject();
          b.addOne(Tuple2.apply(name, value));
        }
        return new MapToBindings((Map) b.result());
      }
    }

    public static record CachedBindings(LibraryName libraryName, MapToBindings bindings, Optional<List<SourceFile<TruffleFile>>> sources) {
    }

    public record Metadata(
            @JsonProperty("source_hash") String sourceHash,
            @JsonProperty("blob_hash") String blobHash) implements Cache.Metadata {}

    private static final String bindingsCacheDataExtension = ".bindings";

    private static final String bindingsCacheMetadataExtension =".bindings.meta";

    private final static ObjectMapper objectMapper = new ObjectMapper();

}
