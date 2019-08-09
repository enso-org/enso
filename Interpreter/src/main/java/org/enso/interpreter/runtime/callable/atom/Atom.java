package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.interop.TruffleObject;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/** A runtime representation of an Atom in Enso. */
public class Atom implements TruffleObject {
  private final AtomConstructor constructor;
  private @CompilationFinal(dimensions = 1) Object[] fields;

  /**
   * Creates a new Atom for a given constructor.
   *
   * @param constructor the Atom's constructor
   * @param fields the Atom's fields
   */
  public Atom(AtomConstructor constructor, Object... fields) {
    this.constructor = constructor;
    this.fields = fields;
  }

  /**
   * Gets the Atom's constructor.
   *
   * @return the constructor for this Atom
   */
  public AtomConstructor getConstructor() {
    return constructor;
  }

  /**
   * Gets the fields from the Atom.
   *
   * @return this Atom's fields
   */
  public Object[] getFields() {
    return fields;
  }

  /**
   * Creates a textual representation of this Atom, useful for debugging.
   *
   * @return a textual representation of this Atom.
   */
  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append(getConstructor().getName());
    builder.append("<");
    List<String> fieldStrings =
        Arrays.stream(fields).map(Object::toString).collect(Collectors.toList());
    builder.append(String.join(", ", fieldStrings));
    builder.append(">");

    return builder.toString();
  }
}
