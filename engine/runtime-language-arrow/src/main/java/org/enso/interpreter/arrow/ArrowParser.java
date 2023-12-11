package org.enso.interpreter.arrow;

import com.oracle.truffle.api.source.Source;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArrowParser {

  public static class Result {
    private final PhysicalLayout physicalLayout;
    private final LogicalLayout logicalLayout;

    private Result(PhysicalLayout physicalLayout, LogicalLayout logicalLayout) {
      this.physicalLayout = physicalLayout;
      this.logicalLayout = logicalLayout;
    }

    public PhysicalLayout getPhysicalLayout() {
      return physicalLayout;
    }

    public LogicalLayout getLogicalLayout() {
      return logicalLayout;
    }
  }

  public static Result parse(Source source) {
    String src = source.getCharacters().toString();
    Matcher m = ArrayPattern.matcher(src);
    if (m.find()) {
      try {
        var layout = LogicalLayout.valueOf(m.group(1));
        return new Result(PhysicalLayout.Primitive, layout);
      } catch (IllegalArgumentException iae) {
        // propagate warning
        return null;
      }
    }
    return null;
  }

  private static final Pattern ArrayPattern = Pattern.compile("new\\[(.+)\\]");

  public enum PhysicalLayout {
    Primitive,
    VariableSizeBinary
  }

  public enum LogicalLayout {
    Date32,
    Date64,
    Float8,
    Float16,
    Float32,
    Float64
  }
}
