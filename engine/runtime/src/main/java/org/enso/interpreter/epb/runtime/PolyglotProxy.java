package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.epb.node.ContextRewrapExceptionNode;
import org.enso.interpreter.epb.node.ContextRewrapNode;

/**
 * Wraps a polyglot value that is to be shared between Truffle contexts. See {@link
 * org.enso.interpreter.epb.EpbLanguage} for the explanation of this system.
 */
@ExportLibrary(InteropLibrary.class)
public class PolyglotProxy implements TruffleObject {
  final Object delegate;
  private final GuardedTruffleContext origin;
  private final GuardedTruffleContext target;

  /**
   * Wraps a value for inter-context use.
   *
   * @param delegate the wrapped value
   * @param origin the context in which {@code delegate} is a valid value
   * @param target the context in which {@code delegate} will be accessed through this wrapper
   */
  public PolyglotProxy(
      Object delegate, GuardedTruffleContext origin, GuardedTruffleContext target) {
    this.delegate = delegate;
    this.origin = origin;
    this.target = target;
  }

  /** @return the wrapped value */
  public Object getDelegate() {
    return delegate;
  }

  /** @return the context in which the wrapped value originates */
  public GuardedTruffleContext getOrigin() {
    return origin;
  }

  /** @return the context in which this wrapper is supposed to be used */
  public GuardedTruffleContext getTarget() {
    return target;
  }

  @ExportMessage
  public boolean isNull(
      @CachedLibrary("this.delegate") InteropLibrary nulls,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return nulls.isNull(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean hasMembers(
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return members.hasMembers(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public Object getMembers(
      boolean includeInternal,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode)
      throws UnsupportedMessageException {
    Object p = origin.enter(node);
    try {
      return contextRewrapNode.execute(
          members.getMembers(this.delegate, includeInternal), origin, target);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean isMemberInvocable(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return members.isMemberInvocable(this.delegate, member);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public Object invokeMember(
      String member,
      Object[] arguments,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary exceptions,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode)
      throws ArityException, UnknownIdentifierException, UnsupportedMessageException,
          UnsupportedTypeException {
    Object[] wrappedArgs = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      wrappedArgs[i] = contextRewrapNode.execute(arguments[i], target, origin);
    }
    Object p = origin.enter(node);
    try {
      return contextRewrapNode.execute(
          members.invokeMember(this.delegate, member, wrappedArgs), origin, target);
    } catch (Throwable e) {
      if (exceptions.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean isMemberReadable(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return members.isMemberReadable(this.delegate, member);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public Object readMember(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode)
      throws UnknownIdentifierException, UnsupportedMessageException {
    Object p = origin.enter(node);
    try {
      return contextRewrapNode.execute(members.readMember(this.delegate, member), origin, target);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean isExecutable(
      @CachedLibrary("this.delegate") InteropLibrary functions,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return functions.isExecutable(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public Object execute(
      Object[] arguments,
      @CachedLibrary("this.delegate") InteropLibrary functions,
      @CachedLibrary("this") InteropLibrary node,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
    Object[] wrappedArgs = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      wrappedArgs[i] = contextRewrapNode.execute(arguments[i], target, origin);
    }
    Object p = origin.enter(node);
    try {
      return contextRewrapNode.execute(
          functions.execute(this.delegate, wrappedArgs), origin, target);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean hasArrayElements(
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return arrays.hasArrayElements(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public long getArraySize(
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node)
      throws UnsupportedMessageException {
    Object p = origin.enter(node);
    try {
      return arrays.getArraySize(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean isArrayElementReadable(
      long idx,
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return arrays.isArrayElementReadable(this.delegate, idx);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public Object readArrayElement(
      long index,
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    Object p = origin.enter(node);
    try {
      return contextRewrapNode.execute(
          arrays.readArrayElement(this.delegate, index), origin, target);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public boolean isString(
      @CachedLibrary("this.delegate") InteropLibrary strings,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return strings.isString(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public String asString(
      @CachedLibrary("this.delegate") InteropLibrary strings,
      @CachedLibrary("this") InteropLibrary node)
      throws UnsupportedMessageException {
    Object p = origin.enter(node);
    try {
      return strings.asString(this.delegate);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  public Object toDisplayString(
      boolean allowSideEffects,
      @CachedLibrary("this.delegate") InteropLibrary displays,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = origin.enter(node);
    try {
      return displays.toDisplayString(this.delegate, allowSideEffects);
    } finally {
      origin.leave(node, p);
    }
  }

  @ExportMessage
  ExceptionType getExceptionType(
      @CachedLibrary("this.delegate") InteropLibrary exceptions,
      @CachedLibrary("this") InteropLibrary node)
      throws UnsupportedMessageException {
    return exceptions.getExceptionType(delegate);
  }

  // TODO ALL THE REST OF EXCEPTION MESSAGES

  // one important caveat – `throwException` should:
  // 1. Delegate to throwException on delegate
  // 2. Catch immediately
  // 3. Wrap in PolyglotExceptionProxy (use ContextRewrapExceptionNode!)
  // 4. Rethrow the wrapped
}
