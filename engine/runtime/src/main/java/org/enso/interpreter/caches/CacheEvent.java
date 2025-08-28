package org.enso.interpreter.caches;

public sealed interface CacheEvent {

  /**
   * Unique name for the cache. For example {@link Cache#logName}.
   *
   * @return not null.
   */
  String cacheName();

  sealed interface Load extends CacheEvent {

    /** Size in bytes. */
    int size();

    /**
     * @return File path or file name. Not null.
     */
    String file();
  }

  record Save(String cacheName, int size) implements CacheEvent {}

  /** Load by reading all bytes from a file. */
  record FileLoad(String cacheName, int size, String file) implements Load {}

  /** Load by mapping file to memory. */
  record MmapLoad(String cacheName, int size, String file) implements Load {}

  record Serialize(String cacheName) implements CacheEvent {}

  record Deserialize(String cacheName) implements CacheEvent {}

  record Invalidate(String cacheName) implements CacheEvent {}
}
