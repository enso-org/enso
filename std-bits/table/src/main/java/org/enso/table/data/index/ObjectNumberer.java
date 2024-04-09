package org.enso.table.data.index;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/** Map objects to serial numbers starting at 0. */
public class ObjectNumberer<T> {
  private final Map<T, Integer> numbering = new HashMap<>();
  private int serial = 0;

  public ObjectNumberer() {}

  public ObjectNumberer(Collection<T> ts) {
    putAll(ts);
  }

  public synchronized void put(T t) {
    if (!numbering.containsKey(t)) {
      numbering.put(t, serial);
      serial++;
    }
  }

  public void putAll(Collection<T> ts) {
    for (T t : ts) {
      put(t);
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
