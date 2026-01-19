package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

public abstract class Ordering {
  Ordering() {}

  public Type getType() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }

  public Atom newEqual() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }

  public Atom newLess() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }

  public Atom newGreater() {
    throw new UnsupportedOperationException(
        "Not supported yet."); // Generated from
                               // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
  }

  public abstract static class Comparable {
    Comparable() {}

    public abstract Type getType();

    public AtomConstructor getBy() {
      throw new UnsupportedOperationException(
          "Not supported yet."); // Generated from
                                 // nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }
  }
}
