package org.enso.persist;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class PerUtils {
  private PerUtils() {}

  static final Logger LOG = LoggerFactory.getLogger(Persistance.class.getPackageName());

  @SuppressWarnings("unchecked")
  static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
