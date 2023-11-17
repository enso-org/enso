package org.enso.persistance;

final class PerUtils {
  private PerUtils() {}

  @SuppressWarnings("unchecked")
  static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
