package org.enso.polyglot;

import java.util.UUID;

/**
 * UUID that is only accessible. Nodes with internal UUID must not be cached.
 *
 * @param uuid unique identifier
 */
public record InternalUUID(UUID uuid) implements RuntimeID {

  @Override
  public boolean isExternal() {
    return false;
  }
}
