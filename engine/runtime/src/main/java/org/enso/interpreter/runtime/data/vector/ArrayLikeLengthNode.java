package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;

public abstract class ArrayLikeLengthNode extends Node {
  public abstract long executeLength(Object arrayLike);

  @NeverDefault
  public static ArrayLikeLengthNode create() {
    return ArrayLikeLengthNodeGen.create();
  }

  //
  // implementation
  //

  @Specialization
  static long arrayLength(Array self) {
    return self.length();
  }

  @Specialization
  static long vectorLongLength(Vector.Long self) {
    return self.getArraySize();
  }

  @Specialization
  static long vectorDoubleLength(Vector.Double self) {
    return self.getArraySize();
  }

  @Specialization
  long vectorLength(
      Vector.Generic self, @Exclusive @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      return self.getArraySize(iop);
    } catch (UnsupportedMessageException ex) {
      throw ArrayPanics.notAnArrayPanic(this, self);
    }
  }

  @Fallback
  long foreignLength(Object self, @Exclusive @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      return iop.getArraySize(self);
    } catch (UnsupportedMessageException ex) {
      throw ArrayPanics.notAnArrayPanic(this, self);
    }
  }
}
