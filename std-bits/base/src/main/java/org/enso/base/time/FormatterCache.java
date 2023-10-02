package org.enso.base.time;

import org.enso.base.Cache;
import org.graalvm.polyglot.Value;

public class FormatterCache extends Cache<FormatterCacheKey, Value> {
  protected FormatterCache(int lruSize) {
    super(lruSize);
  }

  public static FormatterCache SIMPLE_FORMAT = new FormatterCache(DEFAULT_LRU_SIZE);
  public static FormatterCache ISO_WEEK_DATE_FORMAT = new FormatterCache(DEFAULT_LRU_SIZE);
}
