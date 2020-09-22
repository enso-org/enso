package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.util.Objects;

@ExportLibrary(InteropLibrary.class)
public class Text implements TruffleObject {
  private final String string;

  public Text(String string) {
    this.string = string;
  }

  public static Text create(String string) {
    return new Text(string);
  }

  public String getString() {
    return string;
  }

  public byte[] getBytes() {
    return string.getBytes();
  }

  @Override
  public String toString() {
    return getString();
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Text text = (Text) o;
    return Objects.equals(string, text.string);
  }

  @Override
  public int hashCode() {
    return Objects.hash(string);
  }

  @ExportMessage
  boolean isString() {
    return true;
  }

  @ExportMessage
  String asString() {
    return getString();
  }
}
