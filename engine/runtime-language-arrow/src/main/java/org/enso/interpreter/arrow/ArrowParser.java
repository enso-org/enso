package org.enso.interpreter.arrow;

import com.oracle.truffle.api.source.Source;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.enso.interpreter.arrow.runtime.SizeInBytes;

public final class ArrowParser {

  private ArrowParser() {}

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

  public enum PhysicalLayout {
    Primitive,
    VariableSizeBinary
  }

  public enum LogicalLayout implements SizeInBytes {
    Date32(32),
    Date64(64),
    Int8(8),
    Int16(16),
    Int32(32),
    Int64(64);

    private final int bits;

    LogicalLayout(int bits) {
      this.bits = bits;
    }

    @Override
    public int sizeInBytes() {
      return bits / 8;
    }
  }

  public enum Mode {
    Allocate,
    Cast
  }
}
