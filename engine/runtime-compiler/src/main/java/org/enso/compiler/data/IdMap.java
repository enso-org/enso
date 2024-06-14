package org.enso.compiler.data;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.Location;

public final class IdMap {

  private final Map<Location, UUID> values;

  public IdMap(Map<Location, UUID> values) {
    this.values = values;
  }

  public static IdMap empty() {
    return new IdMap(Collections.emptyMap());
  }

  public Map<Location, UUID> getValues() {
    return values;
  }
}
