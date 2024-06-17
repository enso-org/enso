package org.enso.compiler.data;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.Location;

/** A mapping between the code {@link Location}s and their identifiers. */
public record IdMap(Map<Location, UUID> values) {

  public static IdMap empty() {
    return new IdMap(Collections.emptyMap());
  }
}
