package org.enso.interpreter.caches;

import com.oracle.truffle.api.TruffleLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Optional;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.enso.editions.LibraryName;
import org.enso.interpreter.caches.SuggestionsCache.CachedSuggestions;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.enso.polyglot.Suggestion;
import org.enso.version.BuildVersion;

@Persistable(clazz = CachedSuggestions.class, id = 30301)
@Persistable(clazz = Suggestion.Constructor.class, id = 30303)
@Persistable(clazz = Suggestion.Conversion.class, id = 30304)
@Persistable(clazz = Suggestion.DefinedMethod.class, id = 30305)
@Persistable(clazz = Suggestion.Function.class, id = 30306)
@Persistable(clazz = Suggestion.Getter.class, id = 30307)
@Persistable(clazz = Suggestion.Local.class, id = 30308)
@Persistable(clazz = Suggestion.Module.class, id = 30309)
@Persistable(clazz = Suggestion.Type.class, id = 30310)
@Persistable(clazz = Suggestion.Argument.class, id = 30311)
@Persistable(clazz = LibraryName.class, id = 30312)
public final class SuggestionsCache
    implements Cache.Spi<SuggestionsCache.CachedSuggestions, SuggestionsCache.Metadata> {
  private static final String SUGGESTIONS_CACHE_DATA_EXTENSION = ".suggestions";
  private static final String SUGGESTIONS_CACHE_METADATA_EXTENSION = ".suggestions.meta";

  final LibraryName libraryName;

  private SuggestionsCache(LibraryName libraryName) {
    this.libraryName = libraryName;
  }

  public static Cache<SuggestionsCache.CachedSuggestions, SuggestionsCache.Metadata> create(
      LibraryName libraryName) {
    var impl = new SuggestionsCache(libraryName);
    var logName = "Suggestions(" + libraryName + ")";
    return Cache.create(impl, Level.FINE, logName, true, false);
  }

  @Override
  public String metadataSuffix() {
    return SUGGESTIONS_CACHE_METADATA_EXTENSION;
  }

  @Override
  public String dataSuffix() {
    return SUGGESTIONS_CACHE_DATA_EXTENSION;
  }

  @Override
  public String entryName() {
    return libraryName.name();
  }

  @Override
  public byte[] metadata(String sourceDigest, String blobDigest, CachedSuggestions entry)
      throws IOException {
    return new Metadata(sourceDigest, blobDigest).toBytes();
  }

  @Override
  public byte[] serialize(EnsoContext context, CachedSuggestions entry) throws IOException {
    return Persistance.write(entry, CacheUtils.writeReplace(context.getCompiler().context(), true));
  }

  @Override
  public CachedSuggestions deserialize(
      EnsoContext context, ByteBuffer data, Metadata meta, TruffleLogger logger)
      throws IOException {
    var ref = Persistance.read(data, CacheUtils.readResolve(context.getCompiler().context()));
    var cachedSuggestions = ref.get(CachedSuggestions.class);
    return cachedSuggestions;
  }

  @Override
  public Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger)
      throws IOException {
    return Optional.of(Metadata.read(bytes));
  }

  @Override
  public Optional<String> computeDigest(CachedSuggestions entry, TruffleLogger logger) {
    return Optional.of(CacheUtils.computeDigestFromLibName(entry.libraryName));
  }

  @Override
  public Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    return Optional.of(CacheUtils.computeDigestFromLibName(libraryName));
  }

  @Override
  public Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(
            pkg -> {
              var bindingsCacheRoot =
                  pkg.getSuggestionsCacheRootForPackage(BuildVersion.ensoVersion());
              var localCacheRoot = bindingsCacheRoot.resolve(libraryName.namespace());
              var distribution = context.getDistributionManager();
              var pathSegments =
                  new String[] {
                    pkg.namespace(),
                    pkg.normalizedName(),
                    pkg.getConfig().version(),
                    BuildVersion.ensoVersion(),
                    libraryName.namespace()
                  };
              var path =
                  distribution.LocallyInstalledDirectories()
                      .irCacheDirectory()
                      .resolve(StringUtils.join(pathSegments, "/"));
              var globalCacheRoot = context.getTruffleFile(path.toFile());
              return new Cache.Roots(localCacheRoot, globalCacheRoot);
            });
  }

  @Override
  public String sourceHash(Metadata meta) {
    return meta.sourceHash();
  }

  @Override
  public String blobHash(Metadata meta) {
    return meta.blobHash();
  }

  /**
   * @param libraryName
   * @param suggestions Must not be null.
   */
  public record CachedSuggestions(LibraryName libraryName, ArrayList<Suggestion> suggestions) {}

  public record Metadata(String sourceHash, String blobHash) {
    byte[] toBytes() throws IOException {
      try (var os = new ByteArrayOutputStream();
          var dos = new DataOutputStream(os)) {
        dos.writeUTF(sourceHash());
        dos.writeUTF(blobHash());
        return os.toByteArray();
      }
    }

    static Metadata read(byte[] arr) throws IOException {
      try (var is = new ByteArrayInputStream(arr);
          var dis = new DataInputStream(is)) {
        return new Metadata(dis.readUTF(), dis.readUTF());
      }
    }
  }
}
