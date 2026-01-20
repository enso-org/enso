package org.enso.polyglot;

import java.util.UUID;

/** A wrapper around UUIDs differentiating between external and internal identifiers. */
public sealed interface RuntimeID permits InternalUUID, ExternalUUID {
  /** The underlying UUID */
  UUID uuid();

  /** Indicates if UUID represents an externally-visible entity. */
  boolean isExternal();

  /** Indicates if the value of the given UUID will be cached internally. */
  boolean canBeCached();
}
