package org.enso.table.data.column.operation.aggregate;

import org.enso.table.data.column.storage.Storage;

import java.util.stream.IntStream;

/**
 * Represents a fold-like operation on a storage. An aggregator is usually created for a given
 * storage, then {@link #nextGroup(IntStream)} is repeatedly called and the aggregator is
 * responsible for collecting the results of such calls. After that, {@link #seal()} is called to
 * obtain a storage containing all the results.
 */
public abstract class Aggregator {
  /**
   * Requests the aggregator to append the result of aggregating the values at the specified
   * positions.
   *
   * @param positions the positions to aggregate in this round.
   */
  public abstract void nextGroup(IntStream positions);

  /**
   * Returns the results of all previous {@link #nextGroup(IntStream)} calls.
   *
   * @return the storage containing all aggregation results.
   */
  public abstract Storage seal();
}
