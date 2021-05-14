package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;
import org.enso.interpreter.epb.runtime.PolyglotProxy;

@GenerateUncached
@ReportPolymorphism
public abstract class ContextRewrapNode extends Node {

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
  public abstract Object execute(
      Object value, GuardedTruffleContext origin, GuardedTruffleContext target);

  @Specialization
  double doDouble(double d, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return d;
  }

  @Specialization
  double doFloat(float d, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return d;
  }

  @Specialization
  long doLong(long i, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return i;
  }

  @Specialization
  long doInt(int i, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return i;
  }

  @Specialization
  boolean doBoolean(boolean b, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return b;
  }

  @Specialization(guards = "proxy.getOrigin() == target")
  Object doUnwrapProxy(
      PolyglotProxy proxy, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return proxy.getDelegate();
  }

  @Specialization(guards = "proxy.getTarget() == target")
  Object doAlreadyProxied(
      PolyglotProxy proxy, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return proxy;
  }

  @Specialization(guards = "bools.isBoolean(b)")
  boolean doWrappedBoolean(
      Object b,
      GuardedTruffleContext origin,
      GuardedTruffleContext target,
      @CachedLibrary(limit = "5") InteropLibrary bools) {
    try {
      return bools.asBoolean(b);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `b` is checked to be a boolean");
    }
  }

  @Specialization(guards = {"numbers.isNumber(l)", "numbers.fitsInLong(l)"})
  long doWrappedLong(
      Object l,
      GuardedTruffleContext origin,
      GuardedTruffleContext target,
      @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asLong(l);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `l` is checked to be a long");
    }
  }

  @Specialization(
      guards = {"numbers.isNumber(d)", "!numbers.fitsInLong(d)", "numbers.fitsInDouble(d)"})
  double doWrappedDouble(
      Object d,
      GuardedTruffleContext origin,
      GuardedTruffleContext target,
      @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asDouble(d);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `l` is checked to be a double");
    }
  }

  @Fallback
  Object doWrapProxy(Object o, GuardedTruffleContext origin, GuardedTruffleContext target) {
    return new PolyglotProxy(o, origin, target);
  }
}
