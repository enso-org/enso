package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;

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
  static long vectorLength(Vector self, @Exclusive @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      return self.getArraySize(iop);
    } catch (UnsupportedMessageException ex) {
      throw CompilerDirectives.shouldNotReachHere(ex);
    }
  }

  @Specialization
  long foreignLength(Object self, @Exclusive @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      return iop.getArraySize(self);
    } catch (UnsupportedMessageException ex) {
      var ctx = EnsoContext.get(this);
      var err =
          ctx.getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(new Object[] {self}, "Not an array");
      throw new PanicException(err, this);
    }
  }
}
