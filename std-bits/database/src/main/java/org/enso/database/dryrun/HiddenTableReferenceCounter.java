package org.enso.database.dryrun;

import java.util.HashMap;
import java.util.Map;

/**
 * A helper for the Enso part of the registry of hidden tables.
 *
 * <p>It guarantees thread-safety of table reference counting, even when the finalizers are run on a
 * separate thread.
 *
 * <p>This could be implemented more efficiently using a ConcurrentHashMap, but it's just not worth
 * it as the majority of accesses will happen from a single thread, so there is not much benefit
 * from that - a simple `synchronized` implementation will be good enough for the use-case.
 */
public class HiddenTableReferenceCounter {
  private final Map<String, Integer> tableRefCounts = new HashMap<>();

  public synchronized void increment(String name) {
    tableRefCounts.compute(name, (k, c) -> c == null ? 1 : c + 1);
  }

  public synchronized long decrement(String name) {
    Integer x =
        tableRefCounts.compute(
            name,
            (k, c) -> {
              if (c == null) {
                throw new IllegalStateException(
                    "The table "
                        + name
                        + " was not found in the hidden table registry. Reference counter decrement without a paired increment?");
              }

              int newCount = c - 1;
              if (newCount == 0) {
                return null;
              } else {
                return newCount;
              }
            });

    return x == null ? 0 : x;
  }
}
