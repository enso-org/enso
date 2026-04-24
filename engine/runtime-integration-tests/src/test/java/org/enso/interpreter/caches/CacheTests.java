package org.enso.interpreter.caches;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import java.io.IOException;
import java.lang.foreign.Arena;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.logging.Level;
import org.enso.interpreter.caches.Cache.Spi;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.test.utils.ContextUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public final class CacheTests {

  @Rule public final TemporaryFolder tempFolder = new TemporaryFolder();
  @Rule public final ContextUtils ctx = ContextUtils.createDefault();
  private final Random random = new Random(42);

  @Test
  public void cacheCanBeSaved_ToLocalCacheRoot() throws IOException {
    var cacheRoots = createCacheRoots();
    var ensoCtx = ctx.ensoContext();
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var ret = cache.save(new CachedData(), ensoCtx);
    assertThat("was saved to local cache root", ret, is(firstElement(cacheRoots)));
    var localCacheFile =
        firstElement(cacheRoots).resolve(CacheSpi.ENTRY_NAME + CacheSpi.DATA_SUFFIX);
    assertThat("local cache file was created", localCacheFile.exists(), is(true));
  }

  @Test
  public void firstCacheRootIsPreferred() throws IOException {
    var cacheRoots = createCacheRoots();
    var ensoCtx = ctx.ensoContext();
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var ret = cache.save(new CachedData(), ensoCtx);
    assertThat("was saved to first cache root", ret, is(firstElement(cacheRoots)));
    var cacheFileInFirstCacheRoot =
        firstElement(cacheRoots).resolve(CacheSpi.ENTRY_NAME + CacheSpi.DATA_SUFFIX);
    assertThat("first cache file was created", cacheFileInFirstCacheRoot.exists(), is(true));
  }

  @Test
  public void memoryArenaIsClosed_AfterCacheSave() throws IOException {
    var ensoCtx = ctx.ensoContext();
    var cacheRoots = createCacheRoots();
    var spi = new CacheSpi(cacheRoots);
    var bigData = randomBytes(10 * 1024 * 1024);
    saveToLocalRoot(bigData, cacheRoots);

    var memoryArena = new AtomicReference<Arena>();
    Supplier<Arena> arenaSupplier =
        () -> {
          memoryArena.set(Arena.ofConfined());
          return memoryArena.get();
        };
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false, arenaSupplier);
    var loaded = cache.load(ensoCtx);
    assertThat("was loaded", loaded.isPresent(), is(true));
    assertThat("New arena was created", memoryArena.get(), is(notNullValue()));

    var ret = cache.save(new CachedData(), ensoCtx);
    assertThat("was saved", ret, is(notNullValue()));
    assertThat(
        "Memory arena is closed after cache save", memoryArena.get().scope().isAlive(), is(false));
  }

  @Test
  public void cacheCannotBeLoaded_WithoutMetadataOnDisk() throws IOException {
    var ensoCtx = ctx.ensoContext();
    var cacheRoots = createCacheRoots();
    var localCacheFile =
        firstElement(cacheRoots).resolve(CacheSpi.ENTRY_NAME + CacheSpi.DATA_SUFFIX);
    // Saving only data and no metadata
    try (var os = localCacheFile.newOutputStream()) {
      os.write(new byte[] {42});
    }
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var loaded = cache.load(ensoCtx);
    assertThat("Cannot be loaded without metadata on disk", loaded.isPresent(), is(false));
  }

  @Test
  public void byteBufferIsClosed_AfterCacheIsInvalidated() throws IOException {
    var ensoCtx = ctx.ensoContext();
    var cacheRoots = createCacheRoots();
    var bigData = randomBytes(10 * 1024 * 1024);
    saveToLocalRoot(bigData, cacheRoots);
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var loaded = cache.load(ensoCtx);
    var deserializeBuffer = spi.deserializeBuffer;
    assertThat("was loaded", loaded.isPresent(), is(true));

    cache.invalidate(ensoCtx);

    try {
      deserializeBuffer.get();
      fail("Expected IllegalStateException - cannot read from byte buffer anymore");
    } catch (IllegalStateException e) {
      assertThat(e.getMessage(), containsString("closed"));
    }
  }

  @Test
  public void byteBufferIsValid_AfterCacheLoad() throws IOException {
    var ensoCtx = ctx.ensoContext();
    var cacheRoots = createCacheRoots();
    var bigData = randomBytes(10 * 1024 * 1024);
    saveToLocalRoot(bigData, cacheRoots);
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var loaded = cache.load(ensoCtx);
    assertThat("was loaded", loaded.isPresent(), is(true));

    var deserializeBuffer = spi.deserializeBuffer;
    var firstByte = deserializeBuffer.get();
    assertThat("byte buffer is still readable", firstByte, is(bigData[0]));
  }

  @Test
  public void byteBufferIsReadonly() throws IOException {
    var ensoCtx = ctx.ensoContext();
    var cacheRoots = createCacheRoots();
    var bigData = randomBytes(10 * 1024 * 1024);
    saveToLocalRoot(bigData, cacheRoots);
    var spi = new CacheSpi(cacheRoots);
    var cache = Cache.create(spi, Level.FINE, "testCache", false, false);
    var loaded = cache.load(ensoCtx);
    assertThat("was loaded", loaded.isPresent(), is(true));

    var deserializeBuffer = spi.deserializeBuffer;
    try {
      deserializeBuffer.put((byte) 42);
      fail("Expected ReadOnlyBufferException");
    } catch (java.nio.ReadOnlyBufferException e) {
      // expected
    }
  }

  private Iterable<TruffleFile> createCacheRoots() throws IOException {
    var cacheRootDirPath = tempFolder.newFolder("cacheRoot").toPath();
    var localCacheDir = cacheRootDirPath.resolve("local");
    var globalCacheDir = cacheRootDirPath.resolve("global");
    localCacheDir.toFile().mkdir();
    globalCacheDir.toFile().mkdir();
    var ensoCtx = ctx.ensoContext();
    return List.of(
        ensoCtx.getTruffleFile(localCacheDir.toFile()),
        ensoCtx.getTruffleFile(globalCacheDir.toFile()));
  }

  /** Saves data as well as empty metadata on the disk. */
  private static void saveToLocalRoot(byte[] data, Iterable<TruffleFile> cacheRoots)
      throws IOException {
    var localCacheFile =
        firstElement(cacheRoots).resolve(CacheSpi.ENTRY_NAME + CacheSpi.DATA_SUFFIX);
    var localMetadataFile =
        firstElement(cacheRoots).resolve(CacheSpi.ENTRY_NAME + CacheSpi.METADATA_SUFFIX);
    try (var os = localCacheFile.newOutputStream()) {
      os.write(data);
    }
    try (var os = localMetadataFile.newOutputStream()) {
      os.write(42);
    }
  }

  private byte[] randomBytes(int size) {
    byte[] bytes = new byte[size];
    random.nextBytes(bytes);
    return bytes;
  }

  private static TruffleFile firstElement(Iterable<TruffleFile> roots) {
    return roots.iterator().next();
  }

  private static final class CachedData {}

  private static final class Metadata {}

  private static final class CacheSpi implements Spi<CachedData, Metadata> {
    public static final String DATA_SUFFIX = ".test.data";
    public static final String METADATA_SUFFIX = ".test.metadata";
    public static final String ENTRY_NAME = "test-entry";

    private final Iterable<TruffleFile> cacheRoots;
    private ByteBuffer deserializeBuffer;

    private CacheSpi(Iterable<TruffleFile> cacheRoots) {
      this.cacheRoots = cacheRoots;
    }

    @Override
    public CachedData deserialize(
        EnsoContext context, ByteBuffer data, Metadata meta, TruffleLogger logger) {
      deserializeBuffer = data;
      return new CachedData();
    }

    @Override
    public byte[] serialize(EnsoContext context, CachedData entry) {
      return new byte[] {42};
    }

    @Override
    public byte[] metadata(String sourceDigest, String blobDigest, CachedData entry) {
      return new byte[0];
    }

    @Override
    public Metadata metadataFromBytes(byte[] bytes, TruffleLogger logger) {
      return new Metadata();
    }

    @Override
    public Optional<String> computeDigest(CachedData entry, TruffleLogger logger) {
      return Optional.of("42");
    }

    @Override
    public Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
      throw new AssertionError("should not be called");
    }

    @Override
    public Iterable<TruffleFile> getCacheRoots(EnsoContext context) {
      return cacheRoots;
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
