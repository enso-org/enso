package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.table.Column;

import java.util.*;

public class HashIndex extends Index {
  private final Storage items;
  private final Map<Object, List<Integer>> locs;
  private final String name;
  private Index uniqueIndex = null;

  private HashIndex(Storage items, Map<Object, List<Integer>> locs, String name) {
    this.items = items;
    this.locs = locs;
    this.name = name;
  }

  private HashIndex(String name, Storage items, int size) {
    Map<Object, List<Integer>> locations = new HashMap<>();
    for (int i = 0; i < size; i++) {
      List<Integer> its = locations.computeIfAbsent(items.getItemBoxed(i), x -> new ArrayList<>());
      its.add(i);
    }
    this.locs = locations;
    this.items = items;
    this.name = name;
  }

  public static HashIndex fromStorage(String name, Storage storage) {
    return new HashIndex(name, storage, storage.size());
  }

  public Object iloc(int i) {
    return items.getItemBoxed(i);
  }

  @Override
  public List<Integer> loc(Object item) {
    return locs.get(item);
  }

  @Override
  public String ilocString(int loc) {
    return String.valueOf(iloc(loc));
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Column toColumn() {
    return new Column(name, this, items);
  }

  @Override
  public Index mask(BitSet mask, int cardinality) {
    Storage newSt = items.mask(mask, cardinality);
    return HashIndex.fromStorage(name, newSt);
  }

  @Override
  public Index countMask(int[] counts, int total) {
    Storage newSt = items.countMask(counts, total);
    return HashIndex.fromStorage(name, newSt);
  }

  @Override
  public Index applyMask(OrderMask mask) {
    Storage newSt = items.applyMask(mask);
    return HashIndex.fromStorage(name, newSt);
  }

  @Override
  public Index unique() {
    HashMap<Object, List<Integer>> newLocs = new HashMap<>();
    BitSet mask = new BitSet();
    for (int i = 0; i < items.size(); i++) {
      if (!newLocs.containsKey(items.getItemBoxed(i))) {
        newLocs.put(items.getItemBoxed(i), Collections.singletonList(i));
        mask.set(i);
      }
    }
    Storage newItems = items.mask(mask, locs.size());
    return new HashIndex(newItems, newLocs, name);
  }

  @Override
  public int size() {
    return items.size();
  }

  @Override
  public HashIndex slice(int offset, int limit) {
    var newStorage = items.slice(offset, limit);
    return new HashIndex(name, newStorage, newStorage.size());
  }
}
