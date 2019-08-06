package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;

@TypeSystem({long.class, Function.class, Atom.class, AtomConstructor.class})
public class Types {

  @ImplicitCast
  @CompilerDirectives.TruffleBoundary
  public static long castLong(int value) {
    return value;
  }

}
