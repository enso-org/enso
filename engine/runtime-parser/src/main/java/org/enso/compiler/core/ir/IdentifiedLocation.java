package org.enso.compiler.core.ir;

import java.util.UUID;
import scala.Option;

public record IdentifiedLocation(Location location, Option<UUID> id) {
  public IdentifiedLocation(Location location) {
    this(location, Option.empty());
  }

  /** @return the character index of the start of this source location.
  */
  public int start() {
    return location().start();
  }

  /** @return the character index of the end of this source location.
  */
  public int end() {
    return location().end();
  }

  /** @return the length in characters of this location.
  */
  public int length() {
    return location().length();
  }
}
