package org.enso.polyglot;

import java.util.UUID;

/**
 * UUID that is only accessible internally. Nodes with internal UUID may still be cached if they are
 * assigned to an expression that requires it. But their updates will not be reported to clients.
 *
 * @param uuid unique identifier
 * @param cached true, if the expression with the given UUID should be cached
 */
public record InternalUUID(UUID uuid, boolean cached) implements RuntimeID {

  @Override
  public boolean isExternal() {
    return false;
  }

  @Override
  public boolean canBeCached() {
    return cached;
  }

  public static InternalUUID createCached(UUID uuid) {
    return new InternalUUID(uuid, true);
  }

  public static InternalUUID create(UUID uuid) {
    return new InternalUUID(uuid, false);
  }
}
