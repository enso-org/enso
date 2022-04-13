package org.enso.base.text;

public class StringFromBytes {
  public final String result;
  public final String warnings;

  public StringFromBytes(String result) {
    this(result, "");
  }

  public StringFromBytes(String result, String warnings) {
    this.result = result;
    this.warnings = warnings;
  }
}
