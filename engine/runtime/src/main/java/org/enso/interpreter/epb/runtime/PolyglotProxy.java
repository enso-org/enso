package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.epb.node.ContextFlipNode;

@ExportLibrary(InteropLibrary.class)
public class PolyglotProxy implements TruffleObject {
  final Object delegate;
  private final TruffleContext origin;
  private final TruffleContext target;

  public PolyglotProxy(Object delegate, TruffleContext origin, TruffleContext target) {
    this.delegate = delegate;
    this.origin = origin;
    this.target = target;
  }

  public Object getDelegate() {
    return delegate;
  }

  public TruffleContext getOrigin() {
    return origin;
  }

  public TruffleContext getTarget() {
    return target;
  }

  @ExportMessage
  public boolean isNull(@CachedLibrary("this.delegate") InteropLibrary nulls) {
    Object p = origin.enter();
    try {
      return nulls.isNull(this.delegate);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean hasMembers(@CachedLibrary("this.delegate") InteropLibrary members) {
    Object p = origin.enter();
    try {
      return members.hasMembers(this.delegate);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public Object getMembers(
      boolean includeInternal, @CachedLibrary("this.delegate") InteropLibrary members)
      throws UnsupportedMessageException {
    Object p = origin.enter();
    try {
      return members.getMembers(this.delegate, includeInternal);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean isMemberInvocable(
      String member, @CachedLibrary("this.delegate") InteropLibrary members) {
    Object p = origin.enter();
    try {
      return members.isMemberInvocable(this.delegate, member);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public Object invokeMember(
      String member,
      Object[] arguments,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @Cached ContextFlipNode contextFlipNode)
      throws ArityException, UnknownIdentifierException, UnsupportedMessageException,
          UnsupportedTypeException {
    Object p = origin.enter();
    try {
      // TODO Arguments
      return contextFlipNode.execute(
          members.invokeMember(this.delegate, member, arguments), origin, target);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean isMemberReadable(
      String member, @CachedLibrary("this.delegate") InteropLibrary members) {
    Object p = origin.enter();
    try {
      return members.isMemberReadable(this.delegate, member);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public Object readMember(
      String member,
      @CachedLibrary("this.delegate") InteropLibrary members,
      @Cached ContextFlipNode contextFlipNode)
      throws UnknownIdentifierException, UnsupportedMessageException {
    Object p = origin.enter();
    try {
      return contextFlipNode.execute(members.readMember(this.delegate, member), origin, target);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean isExecutable(@CachedLibrary("this.delegate") InteropLibrary functions) {
    Object p = origin.enter();
    try {
      return functions.isExecutable(this.delegate);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public Object execute(
      Object[] arguments,
      @CachedLibrary("this.delegate") InteropLibrary functions,
      @Cached ContextFlipNode contextFlipNode)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
    Object p = origin.enter();
    try {
      return contextFlipNode.execute(functions.execute(this.delegate, arguments), origin, target);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean hasArrayElements(@CachedLibrary("this.delegate") InteropLibrary arrays) {
    Object p = origin.enter();
    try {
      return arrays.hasArrayElements(this.delegate);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public long getArraySize(@CachedLibrary("this.delegate") InteropLibrary arrays)
      throws UnsupportedMessageException {
    Object p = origin.enter();
    try {
      return arrays.getArraySize(this.delegate);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean isArrayElementReadable(
      long idx, @CachedLibrary("this.delegate") InteropLibrary arrays) {
    Object p = origin.enter();
    try {
      return arrays.isArrayElementReadable(this.delegate, idx);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public Object readArrayElement(
      long index,
      @CachedLibrary("this.delegate") InteropLibrary arrays,
      @Cached ContextFlipNode contextFlipNode)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    Object p = origin.enter();
    try {
      return contextFlipNode.execute(arrays.readArrayElement(this.delegate, index), origin, target);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public boolean isString(@CachedLibrary("this.delegate") InteropLibrary strings) {
    Object p = origin.enter();
    try {
      return strings.isString(this.delegate);
    } finally {
      origin.leave(p);
    }
  }

  @ExportMessage
  public String asString(@CachedLibrary("this.delegate") InteropLibrary strings)
      throws UnsupportedMessageException {
    Object p = origin.enter();
    try {
      return strings.asString(this.delegate);
    } finally {
      origin.leave(p);
    }
  }
}
