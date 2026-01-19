package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

public abstract class RuntimeContext {
  private RuntimeContext() {}

  public abstract Type getType();

  public AtomConstructor getOutput() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }

  public AtomConstructor getInput() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }

  public AtomConstructor getDataflowStackTrace() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }
}
