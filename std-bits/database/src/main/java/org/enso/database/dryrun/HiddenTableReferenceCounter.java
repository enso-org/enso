package org.enso.database.dryrun;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * A helper for the Enso part of the registry of hidden tables.
 *
 * <p>It guarantees safety of table reference counting, even when the actions are run from multiple
 * threads.
 *
 * <p>Safety of dropping tables scheduled for removal has to be ensured by the user. Currently, we
 * assume that all database operations only run on the main thread. Thus, once the maintenance
 * starts, no database operation can `increment` a table, thus bringing back a table scheduled for
 * removal. So all tables scheduled for removal are safe to remove as long as no further database
 * operations are performed in the meantime. The only thing that can happen concurrently (because
 * finalizers are run at safepoints interrupting the main thread) is a finalizer marking a table as
 * disposed by calling `decrement` - but this may only make additional tables scheduled for removal.
 * It is fine if such a table is not removed in a currently running maintenance cycle that was
 * interrupted - it will simply be handled by the next cycle.
 */
public class HiddenTableReferenceCounter {
  private final Map<String, Integer> tableRefCounts = new HashMap<>();

  /** Increments the counter for a given table name. */
  public synchronized void increment(String name) {
    tableRefCounts.compute(name, (k, c) -> c == null ? 1 : c + 1);
  }

  /**
   * Decrements the counter for a given table name.
   *
   * <p>If the counter reaches 0, the table is not yet removed but it is scheduled for removal at
   * next maintenance.
   */
  public synchronized void decrement(String name) {
    tableRefCounts.compute(
        name,
        (k, c) -> {
          if (c == null) {
            throw new IllegalStateException(
                "The table "
                    + name
                    + " was not found in the hidden table registry. Reference counter decrement"
                    + " without a paired increment?");
          }

          int newCount = c - 1;
          if (newCount < 0) {
            throw new IllegalStateException(
                "The table "
                    + name
                    + " already had reference count "
                    + c
                    + ", but it was decremented again.");
          } else {
            return newCount;
          }
        });
  }

  /**
   * Checks if the given table name is currently present in the registry.
   *
   * <p>A table is 'registered' even if its reference count has dropped to zero, as long as it has
   * not been disposed yet.
   */
  public synchronized boolean isRegistered(String name) {
    return tableRefCounts.containsKey(name);
  }

  /**
   * Returns the list of tables that have no remaining references and should be removed.
   *
   * <p>Nothing is yet removed from the registry.
   */
  public synchronized List<String> getTablesScheduledForRemoval() {
    return tableRefCounts.entrySet().stream()
        .filter(e -> e.getValue() == 0)
        .map(Map.Entry::getKey)
        .collect(Collectors.toList());
  }

  /**
   * Marks that the table has been successfully dropped. Only tables scheduled for removal should be
   * dropped.
   *
   * <p>No other database operations should be allowed between `getTablesScheduledForRemoval` is
   * invoked and its tables are dropped - as a database operation can 'bring back to life' a table
   * that was scheduled for removal and 'unschedule' it.
   */
  public synchronized void markAsDropped(String name) {
    Integer existingCount = tableRefCounts.remove(name);
    if (existingCount == null) {
      throw new IllegalStateException(
          "Table " + name + " was marked as removed but it was not present in the " + "registry!");
    }

    if (existingCount > 0) {
      throw new IllegalStateException(
          "Table "
              + name
              + " was marked as removed but it still had reference count "
              + existingCount
              + "!");
    }
  }

  /** Returns all tables that were ever added to registry and not yet dropped. */
  public synchronized List<String> getKnownTables() {
    return new ArrayList<>(tableRefCounts.keySet());
  }
}
