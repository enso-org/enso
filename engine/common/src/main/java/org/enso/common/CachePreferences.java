package org.enso.common;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Contains settings telling which nodes are marked for caching and their kinds.
 *
 * @param preferences the config with cached preferences
 */
public record CachePreferences(Map<UUID, Kind> preferences) {

  /**
   * @return empty cache preferences config
   */
  public static CachePreferences empty() {
    return new CachePreferences(new HashMap<>());
  }

  /** A kind of cached value. */
  public enum Kind {
    BINDING_EXPRESSION,
    SELF_ARGUMENT
  }

  /**
   * Get the cache preference for the provided node id.
   *
   * @param id the node id
   * @return the kind of cached value if available
   */
  public Kind get(UUID id) {
    return preferences.get(id);
  }

  /**
   * Get all the node ids marked for caching for a particular node kind.
   *
   * @param kind the node kind
   * @return the set of node ids marked for caching
   */
  public Set<UUID> get(Kind kind) {
    var result = new HashSet<UUID>();
    for (var entry : preferences.entrySet()) {
      if (entry.getValue().equals(kind)) {
        result.add(entry.getKey());
      }
    }

    return result;
  }

  /**
   * Add a cache preference.
   *
   * @param id the node id
   * @param kind the node kind
   */
  public void set(UUID id, Kind kind) {
    preferences.put(id, kind);
  }

  /**
   * Remove the cache preference.
   *
   * @param id the node id to remove
   */
  public void remove(UUID id) {
    preferences.remove(id);
  }

  /**
   * Check if the provided node id is marked for caching.
   *
   * @param id the node id
   * @return return {@code true} if the provided node id is marked for caching
   */
  public boolean contains(UUID id) {
    return preferences.containsKey(id);
  }

  /** Clear all the cache preferences. */
  public void clear() {
    preferences.clear();
  }

  /**
   * Make a copy of this cache preferences.
   *
   * @return a copy of this cache preferences
   */
  public CachePreferences copy() {
    return new CachePreferences(new HashMap<>(preferences));
  }
}
