package org.enso.interpreter.util;

import java.util.Arrays;

public final class ScalaCollections {

  private ScalaCollections() {}

  /**
   * Create a Scala set from the provided elements.
   *
   * @param elems the set elements.
   * @return the immutable Scala set.
   */
  @SafeVarargs
  public static <A> scala.collection.immutable.Set<A> set(A... elems) {
    var s = new scala.collection.mutable.LinkedHashSet<>();
    Arrays.stream(elems).forEach(s::add);
    return s.toSet();
  }


}
