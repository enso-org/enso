package org.enso.compiler.core.ir;

import java.util.UUID;
import scala.Option;

public record IdentifiedLocation(int start, int end, UUID uuid) {

  public IdentifiedLocation(Location location, UUID uuid) {
    this(location.start(), location.end(), uuid);
  }

  public IdentifiedLocation(Location location) {
    this(location.start(), location.end(), null);
  }

  /**
   * @return location of this identified location.
   */
  public Location location() {
    return new Location(start, end);
  }

  /**
   * @return the length in characters of this location.
   */
  public int length() {
    return end - start;
  }

  /**
   * @return option with/out UUID
   */
  public Option<UUID> id() {
    return Option.apply(uuid());
  }

  @Override
  public String toString() {
    return "IdentifiedLocation[start=" + start + ", end=" + end + ", uuid=" + id() + "]";
  }
}
