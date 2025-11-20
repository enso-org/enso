package org.enso.polyglot;

import java.util.UUID;

/**
 * UUID that is only accessible internally. Nodes with internal UUID must never be cached.
 *
 * @param uuid unique identifier
 */
public record InternalUUID(UUID uuid) implements RuntimeID {

  @Override
  public boolean isExternal() {
    return false;
  }

  @Override
  public boolean isCached() {
    return false;
  }
}
