package org.enso.base.cache;

import java.text.DecimalFormat;
import java.text.ParsePosition;

/**
 * Represents a limit on the total size of an LRUCache, either as a fixed byte count or as a
 * percentage of available disk space.
 */
public class TotalCacheLimit {
  /** Parse the limit specification string into either a Bytes or Percentage value. */
  public static Limit parse(String limitString)
      throws IllegalArgumentException, NumberFormatException {
    Number percentageNumber = tryPercentage(limitString);
    if (percentageNumber != null) {
      double percentage = percentageNumber.doubleValue();
      if (percentage < 0.0 || percentage > 1.0) {
        throw new IllegalArgumentException(
            "LURCache free disk space percentage must be in the range 0..100% (inclusive): was "
                + limitString);
      }
      return new Percentage(percentage);
    }
    double megs = Double.parseDouble(limitString);
    long bytes = (long) (megs * 1024 * 1024);
    return new Bytes(bytes);
  }

  public sealed interface Limit permits Bytes, Percentage {}

  // Specify the limit in bytes.
  public record Bytes(long bytes) implements Limit {}

  // Specify the limit as a percentage of total free, usable disk space.
  public record Percentage(double percentage) implements Limit {}

  private static Number tryPercentage(String limitString) {
    DecimalFormat df = new DecimalFormat("0%");
    ParsePosition pp = new ParsePosition(0);
    return df.parse(limitString, pp);
  }
}
