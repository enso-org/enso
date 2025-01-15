package org.enso.example;

import java.time.LocalDate;
import org.graalvm.polyglot.Value;

public final class PolyglotTestClass {
  private PolyglotTestClass() {}

  public static boolean isPolyglotDate_Object(Object obj) {
    return obj instanceof Value polyglotVal && polyglotVal.isDate();
  }

  public static boolean isPolyglotDate_LocalDate(LocalDate date) {
    return date != null;
  }

  public static boolean isPolyglotDate_Value(Value val) {
    return val != null && val.isDate();
  }
}
