package org.enso.base.text;

public class ResultWithWarnings<T> {
  public final T result;
  public final String warnings;

  public ResultWithWarnings(T result) {
    this(result, "");
  }

  public ResultWithWarnings(T result, String warnings) {
    this.result = result;
    this.warnings = warnings;
  }
}
