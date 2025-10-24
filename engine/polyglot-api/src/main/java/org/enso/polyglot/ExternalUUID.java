package org.enso.polyglot;

import java.util.UUID;

/**
 * UUID that is externally accessible. Nodes with external UUID can be cached and visualizations can
 * be attached to them.
 *
 * @param uuid unique identifier
 * @param cached flag indicating if the value of the given UUID will be cached internally
 */
public record ExternalUUID(UUID uuid, boolean cached) implements RuntimeID {

  public ExternalUUID(UUID uuid) {
    this(uuid, true);
  }

  @Override
  public boolean isExternal() {
    return true;
  }

  @Override
  public boolean isCached() {
    return cached;
  }
}
