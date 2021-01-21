package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.epb.runtime.PolyglotProxy;

@GenerateUncached
@ReportPolymorphism
public abstract class ContextFlipNode extends Node {
  /**
   * Wraps a value originating from {@code origin} into a value valid in {@code target}. This method
   * is allowed to use interop library on {@code value} and therefore must be called with {@code
   * origin} entered.
   *
   * @param value
   * @param origin
   * @param target
   * @return
   */
  public abstract Object execute(Object value, TruffleContext origin, TruffleContext target);

  @Specialization
  double doDouble(double d, TruffleContext origin, TruffleContext target) {
    return d;
  }

  @Specialization
  double doFloat(float d, TruffleContext origin, TruffleContext target) {
    return d;
  }

  @Specialization
  long doLong(long i, TruffleContext origin, TruffleContext target) {
    return i;
  }

  @Specialization
  long doInt(int i, TruffleContext origin, TruffleContext target) {
    return i;
  }

  @Specialization(guards = "proxy.getOrigin() == target")
  Object doUnwrapProxy(PolyglotProxy proxy, TruffleContext origin, TruffleContext target) {
    return proxy.getDelegate();
  }

  @Specialization(guards = "proxy.getTarget() == target")
  Object doAlreadyProxied(PolyglotProxy proxy, TruffleContext origin, TruffleContext target) {
    return proxy;
  }

  @Specialization
  Object doWrapProxy(Object o, TruffleContext origin, TruffleContext target) {
    return new PolyglotProxy(o, origin, target);
  }
}
