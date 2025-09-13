package org.enso.persist;

import java.lang.System.Logger;

final class PerUtils {
  private PerUtils() {}

  static final Logger LOG = System.getLogger(Persistance.class.getPackageName());

  @SuppressWarnings("unchecked")
  static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
