package org.enso.interpreter.arrow;

import com.oracle.truffle.api.source.Source;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ArrowParser {

  private ArrowParser() {}

  public record Result(PhysicalLayout physicalLayout, LogicalLayout logicalLayout, Mode mode) {}

  public static Result parse(Source source) {
    String src = source.getCharacters().toString();
    Matcher m = PATTERN.matcher(src);
    if (m.find()) {
      try {
        var layout = LogicalLayout.valueOf(m.group(2));
        var mode = Mode.parse(m.group(1));
        if (layout != null && mode != null) {
          return new Result(PhysicalLayout.Primitive, layout, mode);
        }
      } catch (IllegalArgumentException iae) {
        // propagate warning
      }
    }
    return null;
  }

  private static final Pattern PATTERN = Pattern.compile("^([a-z\\+]+)\\[(.+)\\]$");

  public enum Mode {
    Allocate("new"),
    Cast("cast"),
    Plus("+");

    private final String op;

    private Mode(String text) {
      this.op = text;
    }

    static Mode parse(String operation) {
      for (var m : values()) {
        if (m.op.equals(operation)) {
          return m;
        }
      }
      return null;
    }
  }
}
