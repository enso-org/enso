package org.enso.polyglot;

import java.util.UUID;

/**
 * UUID that is externally accessible. Nodes with external UUID can be cached and visualizations can
 * be attached to them.
 *
 * @param uuid unique identifier
 * @param cached flag indicating if the value of the given UUID will be cached during runtime
 */
public record ExternalUUID(UUID uuid, boolean cached) implements RuntimeID {

  public static RuntimeID createCached(UUID uuid) {
    return new ExternalUUID(uuid, true);
  }

  public static RuntimeID create(UUID uuid) {
    return new ExternalUUID(uuid, false);
  }

  @Override
  public boolean isExternal() {
    return true;
  }

  @Override
  public boolean canBeCached() {
    return cached;
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof ExternalUUID external && external.uuid.equals(uuid);
  }

  @Override
  public int hashCode() {
    return uuid.hashCode();
  }
}
