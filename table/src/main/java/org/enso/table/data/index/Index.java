package org.enso.table.data.index;

import java.util.BitSet;

public abstract class Index {
  public static final int NOT_FOUND = -1;

  public abstract Object iloc(int loc);

  public abstract String ilocString(int loc);

  public abstract int loc(Object loc);

  public abstract String getName();

  public abstract Index mask(BitSet mask, int cardinality);
}
