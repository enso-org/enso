package org.enso.table.data.index;

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
}
