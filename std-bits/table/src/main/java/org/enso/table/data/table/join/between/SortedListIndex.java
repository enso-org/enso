package org.enso.table.data.table.join.between;

import java.util.ArrayList;
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

  public List<T> findSubRange(T lowerBound, T upperBound) {
    int lowerIndex = findLowerIndex(lowerBound);
    int upperIndex = findUpperIndex(upperBound);
    return sortedList.subList(lowerIndex, upperIndex);
  }

  private int findLowerIndex(T lowerBound) {
    // TODO
    return 0;
  }

  private int findUpperIndex(T upperBound) {
    // TODO
    return 0;
  }
}
