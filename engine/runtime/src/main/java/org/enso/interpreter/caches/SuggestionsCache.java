package org.enso.interpreter.caches;

import buildinfo.Info;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.SourceFile;
import org.enso.polyglot.Suggestion;

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
    return Cache.create(impl, Level.FINEST, libraryName.toString(), true, false);
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
  public CachedSuggestions deserialize(
      EnsoContext context, ByteBuffer data, Metadata meta, TruffleLogger logger)
      throws ClassNotFoundException, IOException {
    class BufferInputStream extends InputStream {
      @Override
      public int read() throws IOException {
        return data.get() & 0xff;
      }
    }

    try (var stream = new ObjectInputStream(new BufferInputStream())) {
      if (stream.readObject() instanceof Suggestions suggestions) {
        return new CachedSuggestions(libraryName, suggestions, Optional.empty());
      } else {
        throw new ClassNotFoundException(
            "Expected SuggestionsCache.Suggestions, got " + data.getClass());
      }
    }
  }

  @Override
  public Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger)
      throws IOException {
    return Optional.of(Metadata.read(bytes));
  }

  @Override
  public Optional<String> computeDigest(CachedSuggestions entry, TruffleLogger logger) {
    return entry.getSources().map(sources -> CacheUtils.computeDigestOfLibrarySources(sources));
  }

  @Override
  public Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(pkg -> CacheUtils.computeDigestOfLibrarySources(pkg.listSourcesJava()));
  }

  @Override
  public Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(
            pkg -> {
              var bindingsCacheRoot = pkg.getSuggestionsCacheRootForPackage(Info.ensoVersion());
              var localCacheRoot = bindingsCacheRoot.resolve(libraryName.namespace());
              var distribution = context.getDistributionManager();
              var pathSegments =
                  new String[] {
                    pkg.namespace(),
                    pkg.normalizedName(),
                    pkg.getConfig().version(),
                    Info.ensoVersion(),
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
  public byte[] serialize(EnsoContext context, CachedSuggestions entry) throws IOException {
    var byteStream = new ByteArrayOutputStream();
    try (ObjectOutputStream stream = new ObjectOutputStream(byteStream)) {
      stream.writeObject(entry.getSuggestionsObjectToSerialize());
    }
    return byteStream.toByteArray();
  }

  @Override
  public String sourceHash(Metadata meta) {
    return meta.sourceHash();
  }

  @Override
  public String blobHash(Metadata meta) {
    return meta.blobHash();
  }

  // Suggestions class is not a record because of a Frgaal bug leading to invalid compilation error.
  public static final class Suggestions implements Serializable {

    private final List<Suggestion> suggestions;

    public Suggestions(List<Suggestion> suggestions) {
      this.suggestions = suggestions;
    }

    public List<Suggestion> getSuggestions() {
      return suggestions;
    }
  }

  // CachedSuggestions class is not a record because of a Frgaal bug leading to invalid compilation
  // error.
  public static final class CachedSuggestions {

    private final LibraryName libraryName;
    private final Suggestions suggestions;

    private final Optional<List<SourceFile<TruffleFile>>> sources;

    public CachedSuggestions(
        LibraryName libraryName,
        Suggestions suggestions,
        Optional<List<SourceFile<TruffleFile>>> sources) {
      this.libraryName = libraryName;
      this.suggestions = suggestions;
      this.sources = sources;
    }

    public LibraryName getLibraryName() {
      return libraryName;
    }

    public Optional<List<SourceFile<TruffleFile>>> getSources() {
      return sources;
    }

    public Suggestions getSuggestionsObjectToSerialize() {
      return suggestions;
    }

    public List<Suggestion> getSuggestions() {
      return suggestions.getSuggestions();
    }
  }

  record Metadata(String sourceHash, String blobHash) {
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
