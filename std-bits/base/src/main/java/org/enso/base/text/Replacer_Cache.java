package org.enso.base.text;

import org.enso.base.Cache;

public class Replacer_Cache extends Cache {
  public static Replacer_Cache INSTANCE = new Replacer_Cache(DEFAULT_LRU_SIZE);

  protected Replacer_Cache(int lruSize) {
    super(lruSize);
  }
}
