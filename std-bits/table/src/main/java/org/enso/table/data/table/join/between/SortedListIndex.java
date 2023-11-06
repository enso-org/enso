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
  private int findLowerIndex(T element) {
    int start = 0;
    int end = sortedList.size();
    while (start < end) {
      int mid = Math.addExact(start, end) / 2;
      T midElement = sortedList.get(mid);
      int cmp = comparator.compare(midElement, element);
      if (cmp < 0) {
        start = mid + 1;
      } else {
        end = mid;
      }
    }

    return start;
  }

  /**
   * Finds the index of the last element that is less than or equal to the argument.
   * <p>
   * If all elements are greater than the argument, returns -1.
   * If all elements are less than the argument, returns N-1 (index of the last element).
   */
  private int findUpperIndex(T element) {
    int start = 0;
    int end = sortedList.size();
    while (start < end) {
      int mid = Math.addExact(start, end) / 2;
      T midElement = sortedList.get(mid);
      int cmp = comparator.compare(midElement, element);
      if (cmp > 0) {
        end = mid;
      } else {
        start = mid + 1;
      }
    }

    return end - 1;
  }

  private boolean keysEqual(T k1, T k2) {
    return comparator.compare(k1, k2) == 0;
  }
}
