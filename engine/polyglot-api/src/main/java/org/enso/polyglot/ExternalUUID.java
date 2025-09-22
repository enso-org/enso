package org.enso.polyglot;

import java.util.UUID;

/**
 * UUID that is externally accessible. Nodes with external UUID can be cached and visualizations can
 * be attached to them.
 *
 * @param uuid unique identifier
 */
public record ExternalUUID(UUID uuid) implements RuntimeID {

  @Override
  public boolean isExternal() {
    return true;
  }
}
