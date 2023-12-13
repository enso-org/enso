package org.enso.interpreter.arrow;

import com.oracle.truffle.api.source.Source;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArrowParser {

  public static class Result {

    private final PhysicalLayout physicalLayout;
    private final LogicalLayout logicalLayout;
    private final Mode mode;

    private Result(PhysicalLayout physicalLayout, LogicalLayout logicalLayout, Mode mode) {
      this.physicalLayout = physicalLayout;
      this.logicalLayout = logicalLayout;
      this.mode = mode;
    }

    public PhysicalLayout getPhysicalLayout() {
      return physicalLayout;
    }

    public LogicalLayout getLogicalLayout() {
      return logicalLayout;
    }

    public Mode getAction() {
      return mode;
    }
  }

  public static Result parse(Source source) {
    String src = source.getCharacters().toString();
    Matcher m = ArrayPattern.matcher(src);
    if (m.find()) {
      try {
        var layout = LogicalLayout.valueOf(m.group(1));
        return new Result(PhysicalLayout.Primitive, layout, Mode.Allocate);
      } catch (IllegalArgumentException iae) {
        // propagate warning
        return null;
      }
    }

    m = CastPattern.matcher(src);
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

  private static final Pattern ArrayPattern = Pattern.compile("new\\[(.+)\\]");
  private static final Pattern CastPattern = Pattern.compile("cast\\[(.+)\\]");

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
    Float64,
    Int8,
    Int16,
    Int32,
    Int64
  }

  public enum Mode {
    Allocate,
    Cast
  }
}
