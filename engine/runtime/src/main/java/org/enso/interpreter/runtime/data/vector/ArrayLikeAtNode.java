package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.error.WarningsLibrary;

public abstract class ArrayLikeAtNode extends Node {
  public abstract Object executeAt(Object arrayLike, long index) throws InvalidArrayIndexException;

  @NeverDefault
  public static ArrayLikeAtNode create() {
    return ArrayLikeAtNodeGen.create();
  }

  @NeverDefault
  public static ArrayLikeAtNode getUncached() {
    return ArrayLikeAtNodeGen.getUncached();
  }

  //
  // implementation
  //

  @Specialization
  Object arrayAt(Array self, long index) throws InvalidArrayIndexException {
    try {
      return self.getItems()[Math.toIntExact(index)];
    } catch (ArithmeticException | IndexOutOfBoundsException ex) {
      var cause =
          new AbstractTruffleException(
              ex.getMessage(), ex, AbstractTruffleException.UNLIMITED_STACK_TRACE, this) {};
      throw InvalidArrayIndexException.create(index, cause);
    }
  }

  @Specialization
  Object vectorLongAt(Vector.Long self, long index) throws InvalidArrayIndexException {
    return self.readArrayElement(index);
  }

  @Specialization
  Object vectorDoubleAt(Vector.Double self, long index) throws InvalidArrayIndexException {
    return self.readArrayElement(index);
  }

  @Specialization
  Object vectorAt(
      Vector.Generic self,
      long index,
      @Cached.Exclusive @CachedLibrary(limit = "3") InteropLibrary interop,
      @Cached.Exclusive @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Cached.Exclusive @Cached HostValueToEnsoNode convert)
      throws InvalidArrayIndexException {
    try {
      return self.readArrayElement(index, interop, warnings, convert);
    } catch (UnsupportedMessageException ex) {
      throw ArrayPanics.notAnArrayPanic(this, self);
    }
  }

  @Specialization
  Object genericAt(
      Object self,
      long index,
      @Cached.Exclusive @CachedLibrary(limit = "3") InteropLibrary interop,
      @Cached.Exclusive @Cached HostValueToEnsoNode convert)
      throws InvalidArrayIndexException {
    try {
      var element = interop.readArrayElement(self, index);
      return convert.execute(element);
    } catch (UnsupportedMessageException ex) {
      throw ArrayPanics.notAnArrayPanic(this, self);
    }
  }
}
