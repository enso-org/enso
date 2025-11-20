package org.enso.interpreter.caches;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.foreign.Arena;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.logging.Level;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.logger.masking.MaskedPath;

/**
 * Cache encapsulates a common functionality needed to serialize and de-serialize objects, while
 * maintaining its integrity in the process.
 *
 * @param <T> type of the cached data
 * @param <M> type of the metadata associated with the data
 */
public final class Cache<T, M> {
  private final Object LOCK = new Object();

  /** implementation of the serialize/deserialize operations */
  private final Spi<T, M> spi;

  /** Returns a default level of logging for this Cache. */
  private final Level logLevel;

  /** Log name to use in log messages */
  private final String logName;

  /**
   * Flag indicating if the de-serialization process should compute the hash of the sources from
   * which the cache was created and compare it with the stored metadata entry.
   */
  private final boolean needsSourceDigestVerification;

  /**
   * Large cache files will be {@link FileChannel#map(MapMode, long, long, Arena) mmapped} using a
   * newly created arena via this supplier. Whenever a cache is loaded or saved, the previous arena
   * will be closed, which will invalidate all byte buffers associated with that arena.
   *
   * <p>Note that currently, it is not possible to use {@link Arena#ofConfined()} here. See <a
   * href="https://github.com/enso-org/enso/pull/13872#discussion_r2313983664">GH discussion</a>.
   */
  private final Supplier<Arena> memoryArenaSupplier;

  private Arena memoryArena;

  /**
   * Flag indicating if the de-serialization process should compute the hash of the stored cache and
   * compare it with the stored metadata entry.
   */
  private final boolean needsDataDigestVerification;

  /**
   * Constructor for subclasses.
   *
   * @param logLevel logging level
   * @param logName name to use in logs
   * @param needsSourceDigestVerification Flag indicating if the de-serialization process should
   *     compute the hash of the sources from which the cache was created and compare it with the
   *     stored metadata entry.
   * @param needsDataDigestVerification Flag indicating if the de-serialization process should
   *     compute the hash of the stored cache and compare it with the stored metadata entry.
   */
  private Cache(
      Spi<T, M> spi,
      Level logLevel,
      String logName,
      boolean needsSourceDigestVerification,
      boolean needsDataDigestVerification,
      Supplier<Arena> memoryArenaSupplier) {
    this.spi = spi;
    this.logLevel = logLevel;
    this.logName = logName;
    this.needsDataDigestVerification = needsDataDigestVerification;
    this.needsSourceDigestVerification = needsSourceDigestVerification;
    this.memoryArenaSupplier = memoryArenaSupplier;
  }

  /**
   * Factory method to create new cache instance.
   *
   * @param spi the implementation logic of the cache
   * @param logLevel logging level
   * @param logName name to use in logs
   * @param needsSourceDigestVerification Flag indicating if the de-serialization process should
   *     compute the hash of the sources from which the cache was created and compare it with the
   *     stored metadata entry.
   * @param needsDataDigestVerification Flag indicating if the de-serialization process should
   *     compute the hash of the stored cache and compare it with the stored metadata entry.
   */
  static <T, M> Cache<T, M> create(
      Spi<T, M> spi,
      Level logLevel,
      String logName,
      boolean needsSourceDigestVerification,
      boolean needsDataDigestVerification) {
    return new Cache<>(
        spi,
        logLevel,
        logName,
        needsSourceDigestVerification,
        needsDataDigestVerification,
        Arena::ofShared);
  }

  static <T, M> Cache<T, M> create(
      Spi<T, M> spi,
      Level logLevel,
      String logName,
      boolean needsSourceDigestVerification,
      boolean needsDataDigestVerification,
      Supplier<Arena> memoryArenaSupplier) {
    return new Cache<>(
        spi,
        logLevel,
        logName,
        needsSourceDigestVerification,
        needsDataDigestVerification,
        memoryArenaSupplier);
  }

  /**
   * Saves data to a cache file.
   *
   * @param entry data to save
   * @param context the language context in which loading is taking place
   * @return the location of the successfully saved location of the cached data
   * @throws IOException if something goes wrong
   */
  public final TruffleFile save(T entry, EnsoContext context) throws IOException {
    TruffleLogger logger = context.getLogger(this.getClass());
    for (var root : spi.getCacheRoots(context)) {
      if (saveCacheTo(context, root, entry, logger)) {
        return root;
      }
    }
    throw new IOException("Unable to write cache data for " + logName + ".");
  }

  /**
   * Attempts to save cache data at a specified location.
   *
   * @param cacheRoot parent directory where cache data should be stored
   * @param entry cache data to save
   * @param logger internal logger
   * @return true, if successful, false otherwise
   * @throws IOException IOException encountered while writing data to files
   */
  private boolean saveCacheTo(
      EnsoContext context, TruffleFile cacheRoot, T entry, TruffleLogger logger)
      throws IOException {
    if (ensureRoot(cacheRoot)) {
      byte[] bytesToWrite = spi.serialize(context, entry);

      String blobDigest = CacheUtils.computeDigestFromBytes(ByteBuffer.wrap(bytesToWrite));
      String sourceDigest = spi.computeDigest(entry, logger).get();
      if (sourceDigest == null) {
        throw new IOException("unable to compute digest");
      }
      byte[] metadataBytes = spi.metadata(sourceDigest, blobDigest, entry);

      TruffleFile cacheDataFile = getCacheDataPath(cacheRoot);
      TruffleFile metadataFile = getCacheMetadataPath(cacheRoot);
      TruffleFile parentPath = cacheDataFile.getParent();

      closeMemoryArena();
      if (writeBytesTo(cacheDataFile, bytesToWrite) && writeBytesTo(metadataFile, metadataBytes)) {
        logger.log(
            logLevel,
            "Written cache data [{0}] to [{1}] of size [{2}].",
            new Object[] {logName, toMaskedPath(parentPath).applyMasking(), bytesToWrite.length});
        return true;
      } else {
        // Clean up after ourselves if it fails.
        cacheDataFile.delete();
      }
    }
    return false;
  }

  private boolean ensureRoot(TruffleFile cacheRoot) {
    try {
      if (cacheRoot.exists() && cacheRoot.isDirectory()) {
        return cacheRoot.isWritable();
      } else {
        cacheRoot.createDirectories();
        return cacheRoot.isWritable();
      }
    } catch (Throwable e) {
      return false;
    }
  }

  /**
   * Loads cache for this data, if possible.
   *
   * @param context the language context in which loading is taking place
   * @return the cached data if possible, and [[None]] if it could not load a valid cache
   */
  public final Optional<T> load(EnsoContext context) {
    var logger = context.getLogger(this.getClass());
    var collected = new ArrayList<IOException>();
    synchronized (LOCK) {
      for (var root : spi.getCacheRoots(context)) {
        try {
          var cache = loadCacheFrom(root, context, logger);
          logger.log(
              logLevel,
              "Using cache for ["
                  + logName
                  + "] at location ["
                  + toMaskedPath(root).applyMasking()
                  + "].");
          return Optional.of(cache);
        } catch (IOException ex) {
          collected.add(ex);
        }
      }
    }
    for (var ex : collected) {
      logCacheLoadFailure(logger, "Unable to load a cache [" + logName + "]: ", ex);
    }
    return Optional.empty();
  }

  private void logCacheLoadFailure(TruffleLogger logger, String prefix, IOException ex) {
    if (ex instanceof FileNotFoundException) {
      logger.log(Level.FINE, prefix + ex.getMessage());
    } else {
      logger.log(Level.FINE, prefix + ex.getMessage(), ex);
    }
  }

  /**
   * Loads the cache from the provided `cacheRoot`, invalidating the cache if the loading fails for
   * any reason.
   *
   * @param cacheRoot the root at which to find the cache for this cache entry
   * @param context the language context in which loading is taking place
   * @param logger a logger
   * @return the cached data if available, otherwise throw exception
   */
  private T loadCacheFrom(TruffleFile cacheRoot, EnsoContext context, TruffleLogger logger)
      throws IOException {
    TruffleFile metadataPath = getCacheMetadataPath(cacheRoot);
    TruffleFile dataPath = getCacheDataPath(cacheRoot);

    var meta = loadCacheMetadata(metadataPath, logger);
    if (meta != null) {
      boolean sourceDigestValid =
          !needsSourceDigestVerification
              || spi.computeDigestFromSource(context, logger)
                  .map(digest -> digest.equals(spi.sourceHash(meta)))
                  .orElseGet(() -> false);
      var file = new File(dataPath.toUri());
      ByteBuffer blobBytes;
      var threeMbs = 3 * 1024 * 1024;
      if (file.exists() && file.length() > threeMbs) {
        logger.log(
            Level.FINEST,
            "Cache file {0} mmapped with {1} size",
            new Object[] {file, file.length()});
        closeMemoryArena();
        memoryArena = memoryArenaSupplier.get();
        try (var chan = FileChannel.open(file.toPath())) {
          assert memoryArena.scope().isAlive();
          var memSegment = chan.map(MapMode.READ_ONLY, 0, file.length(), memoryArena);
          assert memSegment.isReadOnly();
          blobBytes = memSegment.asByteBuffer();
        } catch (IOException e) {
          logger.log(Level.SEVERE, "Failed to mmap cache file " + file, e);
          throw e;
        }
      } else {
        blobBytes = ByteBuffer.wrap(dataPath.readAllBytes());
      }
      boolean blobDigestValid =
          !needsDataDigestVerification
              || CacheUtils.computeDigestFromBytes(blobBytes).equals(spi.blobHash(meta));

      if (sourceDigestValid && blobDigestValid) {
        try {
          long now = System.currentTimeMillis();
          var cachedObject = spi.deserialize(context, blobBytes, meta, logger);
          long took = System.currentTimeMillis() - now;
          if (cachedObject != null) {
            logger.log(
                Level.FINEST,
                "Loaded cache for {0} with {1} bytes in {2} ms",
                new Object[] {logName, blobBytes.limit(), took});
            return cachedObject;
          } else {
            invalidateCache(cacheRoot, logger);
            throw new IOException(logName + " is corrupted on disk");
          }
        } catch (ClassNotFoundException ex) {
          invalidateCache(cacheRoot, logger);
          throw new IOException("Cannot load " + dataPath + " due to: " + ex.getMessage(), ex);
        }
      } else {
        invalidateCache(cacheRoot, logger);
        throw new IOException(
            "One or more digests did not match for the cache for [" + logName + "].");
      }

    } else {
      invalidateCache(cacheRoot, logger);
      throw new FileNotFoundException(
          "Could not load the cache metadata at ["
              + toMaskedPath(metadataPath).applyMasking()
              + "].");
    }
  }

  /**
   * Close any previous arena and creates a new one. Closing the previous arena invalidates all byte
   * buffers associated with it.
   */
  private void closeMemoryArena() {
    if (memoryArena != null && memoryArena.scope().isAlive()) {
      memoryArena.close();
    }
  }

  /**
   * Read metadata representation from the provided location
   *
   * @param path location of the serialized metadata
   * @return deserialized metadata, or [[None]] if invalid
   */
  private M loadCacheMetadata(TruffleFile path, TruffleLogger logger) throws IOException {
    if (path.isReadable()) {
      return spi.metadataFromBytes(path.readAllBytes(), logger);
    } else {
      throw new FileNotFoundException("Cannot read " + path);
    }
  }

  /**
   * Gets the path to the cache data within the `cacheRoot`.
   *
   * @param cacheRoot the root of the cache for this entry
   * @return the name of the data file for this entry's cache
   */
  private TruffleFile getCacheDataPath(TruffleFile cacheRoot) {
    return cacheRoot.resolve(cacheFileName(spi.dataSuffix()));
  }

  private TruffleFile getCacheMetadataPath(TruffleFile cacheRoot) {
    return cacheRoot.resolve(cacheFileName(spi.metadataSuffix()));
  }

  /**
   * Computes the cache file name for a given extension.
   *
   * @param suffix the extension
   * @return the cache file name with the provided `ext`
   */
  private String cacheFileName(String suffix) {
    return spi.entryName() + suffix;
  }

  /**
   * Deletes the cache for this data in the provided `cacheRoot`.
   *
   * @param cacheRoot the root of the cache to delete
   * @param logger a logger
   */
  private void invalidateCache(TruffleFile cacheRoot, TruffleLogger logger) {
    TruffleFile metadataFile = getCacheMetadataPath(cacheRoot);
    TruffleFile dataFile = getCacheDataPath(cacheRoot);

    doDeleteAt(cacheRoot, metadataFile, logger);
    doDeleteAt(cacheRoot, dataFile, logger);
  }

  private void doDeleteAt(TruffleFile cacheRoot, TruffleFile file, TruffleLogger logger) {
    try {
      if (file.exists()) {
        if (file.isWritable()) {
          file.delete();
          logger.log(
              logLevel, "Invalidated the cache at [" + toMaskedPath(file).applyMasking() + "].");
        } else {
          logger.log(
              logLevel,
              "Cannot invalidate the cache at ["
                  + toMaskedPath(file).applyMasking()
                  + "]. "
                  + "Cache location not writable.");
        }
      }
    } catch (NoSuchFileException nsfe) {
      // If it doesn't exist, our work has already been done for us!
    } catch (IOException | SecurityException e) {
      logger.log(
          logLevel,
          "Unable to delete the cache at [" + toMaskedPath(cacheRoot).applyMasking() + "].");
    }
  }

  /**
   * Invalidates all caches associated with this cache.
   *
   * @param context the langage context in which loading is taking place
   */
  public final void invalidate(EnsoContext context) {
    synchronized (LOCK) {
      TruffleLogger logger = context.getLogger(this.getClass());
      for (var root : spi.getCacheRoots(context)) {
        invalidateCache(root, logger);
      }
      closeMemoryArena();
    }
  }

  final <T> T asSpi(Class<T> type) {
    return type.cast(spi);
  }

  private static boolean writeBytesTo(TruffleFile file, byte[] bytes) {
    try (OutputStream stream =
        file.newOutputStream(
            StandardOpenOption.WRITE,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING)) {
      stream.write(bytes);
    } catch (IOException ioe) {
      return false;
    } catch (SecurityException se) {
      return false;
    }
    return true;
  }

  private static MaskedPath toMaskedPath(TruffleFile truffleFile) {
    return new MaskedPath(Path.of(truffleFile.getPath()));
  }

  /**
   * Set of methods to be implemented by those who want to cache something.
   *
   * @param <T>
   */
  public static interface Spi<T, M> {
    /**
     * Deserializes and validates data by returning the expected cached entry, or {@code null}.
     *
     * @param context the context
     * @param data data to deserialize object from
     * @param meta metadata corresponding to the `obj`
     * @param logger Truffle's logger
     * @return {@code data} transformed to a cached entry or {@code null}
     * @throws ClassNotFoundException exception thrown on unexpected deserialized data
     * @throws IOException when I/O goes wrong
     */
    public abstract T deserialize(
        EnsoContext context, ByteBuffer data, M meta, TruffleLogger logger)
        throws IOException, ClassNotFoundException;

    /**
     * Returns the exact data to be serialized. Override in subclasses to turn an {@code entry} into
     * an array of bytes to persist
     *
     * @param context context we operate in
     * @param entry entry to persist
     * @return array of bytes
     * @throws java.io.IOException if something goes wrong
     */
    public abstract byte[] serialize(EnsoContext context, T entry) throws IOException;

    /**
     * Return serialized representation of data's metadata.
     *
     * @param sourceDigest digest of data's source
     * @param blobDigest digest of serialized data
     * @param entry data to serialize
     * @return raw bytes representing serialized metadata
     * @throws java.io.IOException in case of I/O error
     */
    public abstract byte[] metadata(String sourceDigest, String blobDigest, T entry)
        throws IOException;

    /**
     * De-serializes raw bytes to data's metadata.
     *
     * @param bytes raw bytes representing metadata
     * @param logger logger to use
     * @return non-empty metadata, if de-serialization was successful
     * @throws IOException in case of I/O error
     */
    public abstract M metadataFromBytes(byte[] bytes, TruffleLogger logger) throws IOException;

    /**
     * Compute digest of cache's data
     *
     * @param entry data for which digest should be computed
     * @param logger Truffle's logger
     * @return non-empty digest, if successful
     */
    public abstract Optional<String> computeDigest(T entry, TruffleLogger logger);

    /**
     * Compute digest of data's source
     *
     * @param context the language context in which loading is taking place
     * @param logger Truffle's logger
     * @return non-empty digest, if successful
     */
    public abstract Optional<String> computeDigestFromSource(
        EnsoContext context, TruffleLogger logger);

    /**
     * Returns locations where caches can be located
     *
     * @param context the language context in which loading is taking place
     * @return non-empty if the locations have been inferred successfully, empty otherwise
     */
    public abstract Iterable<TruffleFile> getCacheRoots(EnsoContext context);

    public abstract String entryName();

    public abstract String dataSuffix();

    public abstract String metadataSuffix();

    public abstract String sourceHash(M meta);

    public abstract String blobHash(M meta);
  }
}
