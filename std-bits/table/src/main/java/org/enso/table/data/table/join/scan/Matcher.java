package org.enso.table.data.table.join.scan;

public interface Matcher {
  boolean matches(int left, int right);
}
