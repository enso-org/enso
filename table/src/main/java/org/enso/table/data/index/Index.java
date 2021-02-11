package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.mask.OrderMask;

import java.util.BitSet;
import java.util.List;

/** A storage class for ordered multisets. */
public abstract class Index {
  public static final int NOT_FOUND = -1;

  /**
   * Returns the element at a given (0-based) position.
   *
   * @param loc the position
   * @return the corresponding element
   */
  public abstract Object iloc(int loc);

  /**
   * Returns a string representation of the item at a given position.
   *
   * @param loc the position
   * @return a string representing the element at the given position
   */
  public abstract String ilocString(int loc);

  /**
   * Returns the list of positions where the given object is contained. The result may be null if
   * the item is not found.
   *
   * @param item the item to lookup
   * @return the list of all positions containing {@code item}
   */
  public abstract List<Integer> loc(Object item);

  /**
   * Builds an index containing the same values as this one, but with only one occurrence of each.
   *
   * @return a unique index obtained from this one.
   */
  public abstract Index unique();

  /** @return the name of this index */
  public abstract String getName();

  /**
   * Return a new index, containing only the items marked true in the mask.
   *
   * @param mask the mask to use
   * @param cardinality the number of true values in mask
   * @return a new index, masked with the given mask
   */
  public abstract Index mask(BitSet mask, int cardinality);

  /**
   * Returns a new index, resulting from applying the rules specified in a mask. The resulting index
   * should contain the elements of the original storage, in the same order. However, the number of
   * consecutive copies of the i-th element of the original index should be {@code counts[i]}.
   *
   * @param counts the mask specifying elements duplication
   * @param total the sum of all elements in the mask, also interpreted as the size of the resulting
   *     index
   * @return the index masked according to the specified rules
   */
  public abstract Index countMask(int[] counts, int total);

  /**
   * Returns a new index, ordered according to the rules specified in a mask.
   *
   * @param mask an order mask specifying the reordering
   * @return an index resulting from applying the reordering rules
   */
  public abstract Index applyMask(OrderMask mask);

  /** @return the number of elements in this index. */
  public abstract int size();
}
