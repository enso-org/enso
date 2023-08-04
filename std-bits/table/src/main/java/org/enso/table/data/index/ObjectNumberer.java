package org.enso.table.data.index;

import java.util.*;

/** Map objects to serial numbers starting at 0. */
public class ObjectNumberer<T> {
  private final Map<T, Integer> numbering = new HashMap<>();
  private int serial = 0;

  public ObjectNumberer() {}

  public ObjectNumberer(Collection<T> ts) {
    addAll(ts);
  }

  public synchronized void add(T t) {
    if (!numbering.containsKey(t)) {
      numbering.put(t, serial);
      serial++;
    }
  }

  public void addAll(Collection<T> ts) {
    for (T t : ts) {
      add(t);
    }
  }

  public int getNumber(T t) {
    return numbering.get(t);
  }

  public int size() {
    return numbering.size();
  }

  public Set<T> getObjects() {
    return numbering.keySet();
  }
}
