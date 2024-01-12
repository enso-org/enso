package org.enso.interpreter.arrow;

import com.oracle.truffle.api.source.Source;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ArrowParser {

  private ArrowParser() {}

  public record Result(PhysicalLayout physicalLayout, LogicalLayout logicalLayout, Mode mode) {}

  public static Result parse(Source source) {
    String src = source.getCharacters().toString();
    Matcher m = ARRAY_PATTERN.matcher(src);
    if (m.find()) {
      try {
        var layout = LogicalLayout.valueOf(m.group(1));
        return new Result(PhysicalLayout.Primitive, layout, Mode.Allocate);
      } catch (IllegalArgumentException iae) {
        // propagate warning
        return null;
      }
    }

    m = CAST_PATTERN.matcher(src);
    if (m.find()) {
      try {
        var layout = LogicalLayout.valueOf(m.group(1));
        return new Result(PhysicalLayout.Primitive, layout, Mode.Cast);
      } catch (IllegalArgumentException iae) {
        // propagate warning
        return null;
      }
    }
    return null;
  }

  private static final Pattern ARRAY_PATTERN = Pattern.compile("new\\[(.+)\\]");
  private static final Pattern CAST_PATTERN = Pattern.compile("cast\\[(.+)\\]");

  public enum Mode {
    Allocate,
    Cast
  }
}
