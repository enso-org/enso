package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Atom implements TruffleObject {
  private final AtomConstructor constructor;
  private final Object[] fields;

  public Atom(AtomConstructor constructor, Object... fields) {
    this.constructor = constructor;
    this.fields = fields;
  }

  public AtomConstructor getConstructor() {
    return constructor;
  }

  public Object[] getFields() {
    return fields;
  }

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
