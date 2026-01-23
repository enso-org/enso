package org.enso.runtime.parser.processor.methodgen;

import org.enso.runtime.parser.processor.GeneratedClassContext;

public final class EqualsMethodGenerator {
  private final GeneratedClassContext ctx;

  public EqualsMethodGenerator(GeneratedClassContext ctx) {
    this.ctx = ctx;
  }

  public String generateMethodCode() {
    var sb = new StringBuilder();
    sb.append("@Override").append(System.lineSeparator());
    sb.append("public boolean equals(Object o) {").append(System.lineSeparator());
    sb.append("  if (this == o) {").append(System.lineSeparator());
    sb.append("    return true;").append(System.lineSeparator());
    sb.append("  }").append(System.lineSeparator());
    sb.append("  if (o instanceof ")
        .append(ctx.getClassName())
        .append(" other) {")
        .append(System.lineSeparator());
    for (var field : ctx.getAllFields()) {
      sb.append(
              "    if (!(Objects.equals(this.$name, other.$name))) {"
                  .replace("$name", field.name()))
          .append(System.lineSeparator());
      sb.append("      return false;").append(System.lineSeparator());
      sb.append("    }").append(System.lineSeparator());
    }
    sb.append("    return true;").append(System.lineSeparator());
    sb.append("  }").append(System.lineSeparator());
    sb.append("  return false;").append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }
}
