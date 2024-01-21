package org.enso.persist;

import java.util.logging.Logger;

final class PerUtils {
  private PerUtils() {}

  static final Logger LOG = Logger.getLogger(Persistance.class.getPackageName());

  @SuppressWarnings("unchecked")
  static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
