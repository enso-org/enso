package org.enso.table.data.index;

import org.enso.table.data.column.storage.StringStorage;

import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;

public class StringIndex extends Index {
  private final Object[] items;
  private final Map<String, Integer> locs;
  private final String name;
  private final int size;

  private StringIndex(Object[] items, Map<String, Integer> locs, String name, int size) {
    this.items = items;
    this.locs = locs;
    this.name = name;
    this.size = size;
  }

  private StringIndex(String name, Object[] items, int start, int size) {
    Map<String, Integer> locations = new HashMap<>();
    for (int i = start; i < size; i++) {
      if (items[i] == null) {
        throw new RuntimeException("Null in index at " + i);
      }
      if (locations.putIfAbsent((String) items[i], i) != null) {
        throw new RuntimeException("Non uniq in index");
      }
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
