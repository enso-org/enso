package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;
import org.enso.interpreter.epb.runtime.PolyglotExceptionProxy;
import org.enso.interpreter.epb.runtime.PolyglotProxy;

@GenerateUncached
@ReportPolymorphism
public abstract class ContextRewrapExceptionNode extends Node {
  /**
   * Wraps a value originating from {@code origin} into a value valid in {@code target}. This method
   * is allowed to use interop library on {@code value} and therefore must be called with {@code
   * origin} entered.
   *
   * @param value the value to wrap
   * @param origin the context the value originates in (and is currently entered)
   * @param target the context in which the value will be accessed in the future
   * @return a context-switch-safe wrapper for the value
   */
  public abstract AbstractTruffleException execute(
      AbstractTruffleException value, GuardedTruffleContext origin, GuardedTruffleContext target);

  @Specialization(guards = "proxy.getOrigin() == target")
  AbstractTruffleException doUnwrapProxy(
      PolyglotExceptionProxy proxy, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return proxy.getOriginal();
  }

  @Specialization(guards = "proxy.getTarget() == target")
  AbstractTruffleException doAlreadyProxied(
      PolyglotExceptionProxy proxy, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return proxy;
  }

  @Fallback
  AbstractTruffleException doWrapProxy(
      AbstractTruffleException o, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return new PolyglotExceptionProxy(o, origin, target);
  }
}
