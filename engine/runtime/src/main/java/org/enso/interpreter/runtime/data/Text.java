package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.TruffleObject;

public class Text implements TruffleObject {
  private final String string;

  public Text(String string) {
    this.string = string;
  }

  public String getString() {
    return string;
  }
}
