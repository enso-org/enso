package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.oracle.truffle.api.TruffleLogger;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Optional;
import java.util.logging.Level;
import org.enso.interpreter.caches.Cache.Roots;
import org.enso.interpreter.caches.Cache.Spi;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.test.utils.ContextUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public final class CacheTests {
  @Rule public final TemporaryFolder tempFolder = new TemporaryFolder();
  @Rule public final ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void cacheCanBeSaved_ToLocalCacheRoot() throws IOException {
    var cacheRoots = createCacheRoots();
    var ensoCtx = ctx.ensoContext();
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var ret = cache.save(new CachedData((byte) 42), ensoCtx, false);
    assertThat("was saved to local cache root", ret, is(cacheRoots.localCacheRoot()));
    var localCacheFile =
        cacheRoots.localCacheRoot().resolve(CacheSpi.ENTRY_NAME + CacheSpi.DATA_SUFFIX);
    assertThat("local cache file was created", localCacheFile.exists(), is(true));
  }

  @Test
  public void globalCacheIsPreferred() throws IOException {
    var cacheRoots = createCacheRoots();
    var ensoCtx = ctx.ensoContext();
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var ret = cache.save(new CachedData((byte) 42), ensoCtx, true);
    assertThat("was saved to global cache root", ret, is(cacheRoots.globalCacheRoot()));
    var globalCacheFile =
        cacheRoots.globalCacheRoot().resolve(CacheSpi.ENTRY_NAME + CacheSpi.DATA_SUFFIX);
    assertThat("global cache file was created", globalCacheFile.exists(), is(true));
  }

  private Roots createCacheRoots() throws IOException {
    var cacheRootDirPath = tempFolder.newFolder("cacheRoot").toPath();
    var localCacheDir = cacheRootDirPath.resolve("local");
    var globalCacheDir = cacheRootDirPath.resolve("global");
    localCacheDir.toFile().mkdir();
    globalCacheDir.toFile().mkdir();
    var ensoCtx = ctx.ensoContext();
    return new Roots(
        ensoCtx.getTruffleFile(localCacheDir.toFile()),
        ensoCtx.getTruffleFile(globalCacheDir.toFile()));
  }

  private record CachedData(byte data) {}

  private static final class Metadata {}

  private static final class CacheSpi implements Spi<CachedData, Metadata> {
    public static final String DATA_SUFFIX = ".test.data";
    public static final String METADATA_SUFFIX = ".test.metadata";
    public static final String ENTRY_NAME = "test-entry";

    private final Roots cacheRoots;

    private CacheSpi(Roots cacheRoots) {
      this.cacheRoots = cacheRoots;
    }

    @Override
    public CachedData deserialize(
        EnsoContext context, ByteBuffer data, Metadata meta, TruffleLogger logger) {
      return new CachedData(data.get(0));
    }

    @Override
    public byte[] serialize(EnsoContext context, CachedData entry) {
      return new byte[] {entry.data};
    }

    @Override
    public byte[] metadata(String sourceDigest, String blobDigest, CachedData entry) {
      return new byte[0];
    }

    @Override
    public Metadata metadataFromBytes(byte[] bytes, TruffleLogger logger) throws IOException {
      return null;
    }

    @Override
    public Optional<String> computeDigest(CachedData entry, TruffleLogger logger) {
      return Optional.of(Byte.toString(entry.data));
    }

    @Override
    public Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
      throw new AssertionError("should not be called");
    }

    @Override
    public Optional<Roots> getCacheRoots(EnsoContext context) {
      return Optional.of(cacheRoots);
    }

    @Override
    public String entryName() {
      return ENTRY_NAME;
    }

    @Override
    public String dataSuffix() {
      return DATA_SUFFIX;
    }

    @Override
    public String metadataSuffix() {
      return METADATA_SUFFIX;
    }

    @Override
    public String sourceHash(Metadata meta) {
      return "42";
    }

    @Override
    public String blobHash(Metadata meta) {
      return "42";
    }
  }
}
