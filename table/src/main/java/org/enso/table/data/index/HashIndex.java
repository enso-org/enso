package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;

import java.util.*;
import java.util.stream.Collectors;

public class HashIndex extends Index {
  private final Object[] items;
  private final Map<Object, List<Integer>> locs;
  private final String name;
  private final int size;

  private HashIndex(Object[] items, Map<Object, List<Integer>> locs, String name, int size) {
    this.items = items;
    this.locs = locs;
    this.name = name;
    this.size = size;
  }

  private HashIndex(String name, Object[] items, int size) {
    Map<Object, List<Integer>> locations = new HashMap<>();
    for (int i = 0; i < size; i++) {
      List<Integer> its = locations.computeIfAbsent(items[i], x -> new ArrayList<>());
      its.add(i);
    }
    this.locs = locations;
    this.items = items;
    this.name = name;
    this.size = size;
  }

  public static HashIndex fromStorage(String name, Storage storage) {
    Object[] data = new Object[(int) storage.size()];
    for (int i = 0; i < storage.size(); i++) {
      data[i] = storage.getItemBoxed(i);
    }
    return new HashIndex(name, data, (int) storage.size());
  }

  public Object iloc(int i) {
    return items[i];
  }

  @Override
  public List<Integer> loc(Object item) {
    return locs.get(item);
  }

  @Override
  public String ilocString(int loc) {
    return iloc(loc).toString();
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Index mask(BitSet mask, int cardinality) {
    Map<Object, List<Integer>> newLocs = new HashMap<>();
    for (Map.Entry<Object, List<Integer>> entry : locs.entrySet()) {
      List<Integer> newIxes =
          entry.getValue().stream().filter(mask::get).collect(Collectors.toList());
      if (!newIxes.isEmpty()) {
        newLocs.put(entry.getKey(), newIxes);
      }
    }
    Object[] newItems = new Object[cardinality];
    int j = 0;
    for (int i = 0; i < size; i++) {
      if (mask.get(i)) {
        newItems[j++] = items[i];
      }
    }
    return new HashIndex(newItems, newLocs, name, cardinality);
  }

  @Override
  public Index countMask(int[] counts, int total) {
    Object[] newItems = new Object[total];
    int pos = 0;
    for (int i = 0; i < size; i++) {
      for (int j = 0; j < counts[i]; j++) {
        newItems[pos++] = items[i];
      }
    }
    return new HashIndex(name, newItems, total);
  }
}
