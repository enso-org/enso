package org.enso.common;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class CachePreferences {

  private final Map<UUID, Kind> preferences;

  private CachePreferences(Map<UUID, Kind> preferences) {
    this.preferences = preferences;
  }

  public CachePreferences() {
    this.preferences = new HashMap<>();
  }

  public enum Kind {
    BINDING_EXPRESSION,
    SELF_ARGUMENT
  }

  public Map<UUID, Kind> getValues() {
    return preferences;
  }

  public Kind get(UUID id) {
    return preferences.get(id);
  }

  public Set<UUID> get(Kind kind) {
    var result = new HashSet<UUID>();
    for (var entry : preferences.entrySet()) {
      if (entry.getValue().equals(kind)) {
        result.add(entry.getKey());
      }
    }

    return result;
  }

  public void set(UUID id, Kind kind) {
    preferences.put(id, kind);
  }

  public void remove(UUID id) {
    preferences.remove(id);
  }

  public boolean contains(UUID id) {
    return preferences.containsKey(id);
  }

  public void clear() {
    preferences.clear();
  }

  public CachePreferences copy() {
    return new CachePreferences(new HashMap<>(preferences));
  }
}
