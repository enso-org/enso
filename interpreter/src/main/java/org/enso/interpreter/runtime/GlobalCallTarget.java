package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.interop.TruffleObject;

public class GlobalCallTarget implements TruffleObject {

  @CompilationFinal private RootCallTarget target;

  public GlobalCallTarget(RootCallTarget target) {
    this.target = target;
  }

  public RootCallTarget getTarget() {
    return target;
  }

  public void setTarget(RootCallTarget target) {
    this.target = target;
  }
}
