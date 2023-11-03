package org.enso.table.data.table.join.between;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class SortedListIndex<T> {
  private final Comparator<T> comparator;
  private final ArrayList<T> sortedList;

  protected SortedListIndex(ArrayList<T> sortedList, Comparator<T> comparator) {
    this.comparator = comparator;
    this.sortedList = sortedList;
  }

  public static <T> SortedListIndex<T> build(List<T> list, Comparator<T> comparator) {
    ArrayList<T> copy = new ArrayList<>(list);
    copy.sort(comparator);
    return new SortedListIndex<>(copy, comparator);
  }

  /**
   * Finds a sub-range of the index containing all elements between the lower and upper bounds (both-ends inclusive).
   */
  public List<T> findSubRange(T lowerBound, T upperBound) {
    int start = findLowerIndex(lowerBound);
    int end = findUpperIndex(upperBound) + 1;
    if (start >= end) {
      return Collections.emptyList();
    }

    return sortedList.subList(start, end);
  }

  /**
   * Finds the index of the first element that is greater than or equal to the argument.
   * <p>
   * If all elements are greater than the argument, returns 0.
   * If all elements are less than the argument, returns N.
   */
  private int findLowerIndex(T lowerBound) {
    int i = Collections.binarySearch(sortedList, lowerBound, comparator);
    if (i < 0) {
      return -i - 1;
    }

    while (i > 0 && keysEqual(sortedList.get(i - 1), lowerBound)) {
      i--;
    }

    return i;
  }

  /**
   * Finds the index of the last element that is less than or equal to the argument.
   * <p>
   * If all elements are greater than the argument, returns -1.
   * If all elements are less than the argument, returns N-1 (index of the last element).
   */
  private int findUpperIndex(T upperBound) {
    int i = Collections.binarySearch(sortedList, upperBound, comparator);
    if (i < 0) {
      // insertion point is the _first element > than the key_
      // we want the last element <= - so we need to move back by one
      // if the first element (index 0) is > than the key, the 'last' <= element is a 'virtual' element at -1
      return -i - 2;
    }

    while (i < sortedList.size() - 1 && keysEqual(sortedList.get(i + 1), upperBound)) {
      i++;
    }

    return i;
  }

  private boolean keysEqual(T k1, T k2) {
    return comparator.compare(k1, k2) == 0;
  }
}
