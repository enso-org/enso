package org.enso.compiler.pass.analyse.types.util;

import java.util.AbstractList;
import java.util.List;
import java.util.function.Function;

/** A list that wraps elements of another list with a function.
 * <p>
 * It assumes that the function is cheap to compute, so it is actually computed on each access. It is meant to be used by lightweight adapter wrappers.
 */
public class ProxyList<Source, Target> extends AbstractList<Target> {
  private final Function<Source, Target> cheapWrapper;
  private final List<Source> source;

  public ProxyList(List<Source> source, Function<Source, Target> cheapWrapper) {
    this.cheapWrapper = cheapWrapper;
    this.source = source;
  }

  @Override
  public Target get(int index) {
    return cheapWrapper.apply(source.get(index));
  }

  @Override
  public int size() {
    return source.size();
  }
}
