package org.enso.compiler;

import buildinfo.Info;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.SourceFile;
import org.enso.polyglot.Suggestion;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;

public final class SuggestionsCache
    extends Cache<SuggestionsCache.CachedSuggestions, SuggestionsCache.Metadata> {

  private static final String SUGGESTIONS_CACHE_DATA_EXTENSION = ".suggestions";
  private static final String SUGGESTIONS_CACHE_METADATA_EXTENSION =".suggestions.meta";

  private final static ObjectMapper objectMapper = new ObjectMapper();

  final LibraryName libraryName;

  public SuggestionsCache(LibraryName libraryName) {
    super(Level.FINEST, libraryName.toString(), true, false);
    this.libraryName = libraryName;
    this.entryName = libraryName.name();
    this.dataSuffix = SUGGESTIONS_CACHE_DATA_EXTENSION;
    this.metadataSuffix = SUGGESTIONS_CACHE_METADATA_EXTENSION;
  }

  @Override
  protected byte[] metadata(String sourceDigest, String blobDigest, CachedSuggestions entry) {
    try {
      return objectMapper.writeValueAsString(new Metadata(sourceDigest, blobDigest)).getBytes(metadataCharset);
    } catch (JsonProcessingException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  protected CachedSuggestions deserialize(EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger)
  throws ClassNotFoundException, ClassNotFoundException, IOException {
    try (var stream = new ObjectInputStream(new ByteArrayInputStream(data))) {
      if (stream.readObject() instanceof Suggestions suggestions) {
        return new CachedSuggestions(libraryName, suggestions, Optional.empty());
      } else {
        throw new ClassNotFoundException("Expected SuggestionsCache.Suggestions, got " + data.getClass());
      }
    }
  }

  @Override
  protected Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger) {
    var maybeJsonString = new String(bytes, Cache.metadataCharset);
    try {
      return Optional.of(objectMapper.readValue(maybeJsonString, SuggestionsCache.Metadata.class));
    } catch (JsonProcessingException e) {
      logger.log(logLevel, "Failed to deserialize suggestions' metadata.", e);
      return Optional.empty();
    }
  }

  @Override
  protected Optional<String> computeDigest(CachedSuggestions entry, TruffleLogger logger) {
    return entry.getSources().map(sources -> computeDigestOfLibrarySources(sources, logger));
  }

  @Override
  protected Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(pkg -> computeDigestOfLibrarySources(pkg.listSourcesJava(), logger));
  }


  @Override
  protected Optional<Roots> getCacheRoots(EnsoContext context) {
    return context.getPackageRepository().getPackageForLibraryJava(libraryName).map(pkg -> {
      var bindingsCacheRoot = pkg.getSuggestionsCacheRootForPackage(Info.ensoVersion());
      var localCacheRoot = bindingsCacheRoot.resolve(libraryName.namespace());
      var distribution = context.getDistributionManager();
      var pathSegments = new String[]{
              pkg.namespace(),
              pkg.name(),
              pkg.config().version(),
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
  protected byte[] serialize(EnsoContext context, CachedSuggestions entry) throws IOException {
    var byteStream = new ByteArrayOutputStream();
    try (ObjectOutputStream stream = new ObjectOutputStream(byteStream)) {
      stream.writeObject(entry.getSuggestionsObjectToSerialize());
    }
    return byteStream.toByteArray();
  }

  // Suggestions class is not a record because of a Frgaal bug leading to invalid compilation error.
  final static class Suggestions implements Serializable {

    private final List<Suggestion> suggestions;

    public Suggestions(List<Suggestion> suggestions)  {
      this.suggestions = suggestions;
    }

    public List<Suggestion> getSuggestions() {
      return suggestions;
    }
  }

  // CachedSuggestions class is not a record because of a Frgaal bug leading to invalid compilation error.
  public final static class CachedSuggestions {

    private final LibraryName libraryName;
    private final Suggestions suggestions;

    private final Optional<List<SourceFile<TruffleFile>>> sources;

    public CachedSuggestions(LibraryName libraryName, Suggestions suggestions, Optional<List<SourceFile<TruffleFile>>> sources) {
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

  record Metadata(
      @JsonProperty("source_hash") String sourceHash,
      @JsonProperty("blob_hash") String blobHash
  ) implements Cache.Metadata { }
}
