package org.enso.table.data.index;

import org.enso.table.data.column.storage.StringStorage;

import java.util.*;

public class StringIndex extends Index {
  private final Object[] items;
  private final Map<String, List<Integer>> locs;
  private final String name;
  private final int size;

  private StringIndex(Object[] items, Map<String, List<Integer>> locs, String name, int size) {
    this.items = items;
    this.locs = locs;
    this.name = name;
    this.size = size;
  }

  private StringIndex(String name, Object[] items, int start, int size) {
    Map<String, List<Integer>> locations = new HashMap<>();
    for (int i = start; i < size; i++) {
      List<Integer> its = locations.computeIfAbsent((String) items[i], x -> new ArrayList<>());
      its.add(i);
    }
    this.locs = locations;
    this.items = items;
    this.name = name;
    this.size = size;
  }

  public static StringIndex fromStorage(String name, StringStorage storage) {
    return new StringIndex(name, storage.getData(), 0, (int) storage.size());
  }

  public String iloc(int i) {
    return (String) items[i];
  }

  @Override
  public int loc(Object i) {
    Integer r = locs.get(i);
    return r == null ? NOT_FOUND : r;
  }

  @Override
  public String ilocString(int loc) {
    return iloc(loc);
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Index mask(BitSet mask, int cardinality) {
    Map<String, Integer> newLocs = new HashMap<>(locs);
    Object[] newItems = new Object[cardinality];
    int j = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        newItems[j++] = items[i];
      } else {
        newLocs.remove(items[i]);
      }
    }
    return new StringIndex(newItems, newLocs, name, cardinality);
  }
}
