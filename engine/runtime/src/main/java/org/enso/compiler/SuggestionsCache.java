package org.enso.compiler;

import buildinfo.Info;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.oracle.truffle.api.TruffleLogger;
import org.bouncycastle.util.encoders.Hex;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.Suggestion;
import scala.jdk.CollectionConverters;

import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;

public final class SuggestionsCache
    extends Cache<SuggestionsCache.CachedSuggestions, SuggestionsCache.Metadata> {

  private static final String SUGGESTIONS_CACHE_DATA_EXTENSION = ".suggestions";
  private static final String SUGGESTIONS_CACHE_METADATA_EXTENSION =".suggestions.meta";

  final LibraryName libraryName;

  public SuggestionsCache(LibraryName libraryName) {
    this.libraryName = libraryName;
    this.logLevel = Level.FINEST;
    this.stringRepr = libraryName.toString();
    this.entryName = libraryName.name();
    this.dataSuffix = SUGGESTIONS_CACHE_DATA_EXTENSION;
    this.metadataSuffix = SUGGESTIONS_CACHE_METADATA_EXTENSION;
  }

  @Override
  protected byte[] metadata(String sourceDigest, String blobDigest, CachedSuggestions entry) {
    var mapper = new ObjectMapper();
    try {
      return mapper.writeValueAsString(new Metadata(sourceDigest, blobDigest)).getBytes(metadataCharset);
    } catch (JsonProcessingException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  protected CachedSuggestions validateReadObject(Object obj, Metadata meta, TruffleLogger logger)
      throws CacheException {
    if (obj instanceof Suggestions suggestions) {
      return new CachedSuggestions(libraryName, suggestions);
    } else {
      throw new CacheException("Expected SuggestionsCache.Suggestions, got " + obj.getClass());
    }
  }

  @Override
  protected Optional<Metadata> metadataFromBytes(byte[] bytes) {
    var maybeJsonString = new String(bytes, Cache.metadataCharset);
    var mapper = new ObjectMapper();
    try {
      return Optional.of(mapper.readValue(maybeJsonString, SuggestionsCache.Metadata.class));
    } catch (JsonProcessingException e) {
      return Optional.empty();
    }
  }

  @Override
  protected Optional<String> computeDigest(CachedSuggestions entry, TruffleLogger logger) {
    var digest = messageDigest();
    entry.getSuggestions().getSuggestions().forEach(suggestion -> {
      ByteBuffer bytes = ByteBuffer.allocate(Integer.BYTES).putInt(suggestion.hashCode());
      digest.update(bytes);
    });
    return Optional.of(Hex.toHexString(digest.digest()));
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
      var pathSegments = CollectionConverters.ListHasAsScala(Arrays.asList(
              pkg.namespace(),
              pkg.name(),
              pkg.config().version(),
              Info.ensoVersion(),
              libraryName.namespace())
      ).asScala();
      var path = distribution.LocallyInstalledDirectories().irCacheDirectory()
              .resolve(pathSegments.mkString("/"));
      var globalCacheRoot = context.getTruffleFile(path.toFile());
      return new Cache.Roots(localCacheRoot, globalCacheRoot);
    });
  }

  @Override
  protected Object extractObjectToSerialize(CachedSuggestions entry) {
    return entry.getSuggestions();
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
  final static class CachedSuggestions {

    private final LibraryName libraryName;
    private final Suggestions suggestions;

    public CachedSuggestions(LibraryName libraryName, Suggestions suggestions) {
      this.libraryName = libraryName;
      this.suggestions = suggestions;
    }

    public LibraryName getLibraryName() {
      return libraryName;
    }

    public Suggestions getSuggestions() {
      return suggestions;
    }
  }

  record Metadata(
      @JsonProperty("source_hash") String sourceHash,
      @JsonProperty("blob_hash") String blobHash
  ) implements Cache.Metadata { }
}
