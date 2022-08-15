package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.time.LocalDate;
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

  Object enterOrigin(InteropLibrary n) {
    if (n.isAdoptable()) {
      return origin.enter(n);
    } else {
      return origin.enter(null);
    }
  }

  void leaveOrigin(InteropLibrary node, Object prev) {
    if (node.isAdoptable()) {
      origin.leave(node, prev);
    } else {
      origin.leave(null, prev);
    }
  }

  @ExportMessage
  public boolean isNull(
      @CachedLibrary("this.delegate") InteropLibrary nulls,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return nulls.isNull(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean hasMembers(
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return members.hasMembers(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public Object getMembers(
      boolean includeInternal,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return contextRewrapNode.execute(
          members.getMembers(this.delegate, includeInternal), origin, target);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean isMemberInvocable(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return members.isMemberInvocable(this.delegate, member);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public Object invokeMember(
      String member,
      Object[] arguments,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws ArityException, UnknownIdentifierException, UnsupportedMessageException,
          UnsupportedTypeException {
    Object[] wrappedArgs = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      wrappedArgs[i] = contextRewrapNode.execute(arguments[i], target, origin);
    }
    Object p = enterOrigin(node);
    try {
      return contextRewrapNode.execute(
          members.invokeMember(this.delegate, member, wrappedArgs), origin, target);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean isMemberReadable(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return members.isMemberReadable(this.delegate, member);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public Object readMember(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnknownIdentifierException, UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return contextRewrapNode.execute(members.readMember(this.delegate, member), origin, target);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean isExecutable(
      @CachedLibrary("this.delegate") InteropLibrary functions,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return functions.isExecutable(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public Object execute(
      Object[] arguments,
      @CachedLibrary("this.delegate") InteropLibrary functions,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
    Object[] wrappedArgs = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      wrappedArgs[i] = contextRewrapNode.execute(arguments[i], target, origin);
    }
    Object p = enterOrigin(node);
    try {
      return contextRewrapNode.execute(
          functions.execute(this.delegate, wrappedArgs), origin, target);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean hasArrayElements(
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return arrays.hasArrayElements(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public long getArraySize(
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return arrays.getArraySize(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean isArrayElementReadable(
      long idx,
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return arrays.isArrayElementReadable(this.delegate, idx);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public Object readArrayElement(
      long index,
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return contextRewrapNode.execute(
          arrays.readArrayElement(this.delegate, index), origin, target);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean isString(
      @CachedLibrary("this.delegate") InteropLibrary strings,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return strings.isString(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public String asString(
      @CachedLibrary("this.delegate") InteropLibrary strings,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return strings.asString(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean isNumber(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.isNumber(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean fitsInByte(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.fitsInByte(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean fitsInShort(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.fitsInShort(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean fitsInInt(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.fitsInInt(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean fitsInLong(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.fitsInLong(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean fitsInFloat(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.fitsInFloat(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public boolean fitsInDouble(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return numbers.fitsInDouble(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public byte asByte(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return numbers.asByte(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public short asShort(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return numbers.asShort(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public int asInt(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return numbers.asInt(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public long asLong(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return numbers.asLong(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public float asFloat(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return numbers.asFloat(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public double asDouble(
      @CachedLibrary("this.delegate") InteropLibrary numbers,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return numbers.asDouble(this.delegate);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  public Object toDisplayString(
      boolean allowSideEffects,
      @CachedLibrary("this.delegate") InteropLibrary displays,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode,
      @Cached @Cached.Exclusive BranchProfile profile) {
    Object p = enterOrigin(node);
    try {
      return displays.toDisplayString(this.delegate, allowSideEffects);
    } catch (Throwable e) {
      profile.enter();
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  boolean isException(
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary("this.delegate") InteropLibrary errors) {
    Object p = enterOrigin(node);
    try {
      return errors.isException(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  RuntimeException throwException(
      @CachedLibrary("this.delegate") InteropLibrary delegate,
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary(limit = "5") InteropLibrary errors,
      @Cached @Cached.Exclusive ContextRewrapExceptionNode contextRewrapExceptionNode)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      throw delegate.throwException(delegate);
    } catch (Throwable e) {
      if (errors.isException(e)) {
        // `isException` means this must be AbstractTruffleException
        //noinspection ConstantConditions
        throw contextRewrapExceptionNode.execute((AbstractTruffleException) e, origin, target);
      } else {
        throw e;
      }
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  ExceptionType getExceptionType(
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary("this.delegate") InteropLibrary errors)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return errors.getExceptionType(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  int getExceptionExitStatus(
      @CachedLibrary("this") InteropLibrary node,
      @CachedLibrary("this.delegate") InteropLibrary errors)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return errors.getExceptionExitStatus(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  boolean isExceptionIncompleteSource(
      @CachedLibrary("this.delegate") InteropLibrary errors,
      @CachedLibrary("this") InteropLibrary node)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return errors.isExceptionIncompleteSource(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  boolean hasExceptionMessage(
      @CachedLibrary("this.delegate") InteropLibrary errors,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = enterOrigin(node);
    try {
      return errors.hasExceptionMessage(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  Object getExceptionMessage(
      @CachedLibrary("this.delegate") InteropLibrary errors,
      @CachedLibrary("this") InteropLibrary node,
      @Cached @Cached.Exclusive ContextRewrapNode contextRewrapNode)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return contextRewrapNode.execute(errors.getExceptionMessage(delegate), origin, target);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  boolean isDate(
      @CachedLibrary("this.delegate") InteropLibrary datum,
      @CachedLibrary("this") InteropLibrary node) {
    Object p = enterOrigin(node);
    try {
      return datum.isDate(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }

  @ExportMessage
  LocalDate asDate(
      @CachedLibrary("this.delegate") InteropLibrary datume,
      @CachedLibrary("this") InteropLibrary node)
      throws UnsupportedMessageException {
    Object p = enterOrigin(node);
    try {
      return datume.asDate(delegate);
    } finally {
      leaveOrigin(node, p);
    }
  }
}
