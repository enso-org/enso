package org.enso.base.text;

import org.enso.base.Cache;
import org.graalvm.polyglot.Value;

public class Replacer_Cache extends Cache<String, Value> {
  public static Replacer_Cache INSTANCE = new Replacer_Cache(DEFAULT_LRU_SIZE);

  protected Replacer_Cache(int lruSize) {
    super(lruSize);
  }
}
