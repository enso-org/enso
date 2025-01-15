package org.enso.runtime.parser.processor.methodgen;

import java.util.List;
import java.util.stream.Collectors;
import org.enso.runtime.parser.processor.GeneratedClassContext;

public final class CopyMethodGenerator {
  private final GeneratedClassContext ctx;

  public CopyMethodGenerator(GeneratedClassContext ctx) {
    this.ctx = ctx;
  }

  /** Generates the default {@code copy} method, with all the fields as parameters. */
  public String generateMethodCode() {
    var docs =
        """
        /**
         * Creates a shallow copy of this IR element. If all of the given parameters are the
         * same objects as fields, no copy is created and {@code this} is returned.
         *
         * <p>As opposed to the {@code duplicate} method,
         * does not copy this IR element recursively.
         */
        """;
    var sb = new StringBuilder();
    sb.append(docs);
    var paramList = String.join(", ", parameters());
    sb.append("public ")
        .append(copyMethodRetType())
        .append(" copy(")
        .append(paramList)
        .append(") {")
        .append(System.lineSeparator());
    sb.append("  ")
        .append("boolean cond = ")
        .append(cond())
        .append(";")
        .append(System.lineSeparator());
    sb.append("  ").append("if (cond) {").append(System.lineSeparator());
    sb.append("    ")
        .append("// One of the parameters is a different object than the field.")
        .append(System.lineSeparator());
    sb.append("    ").append("var bldr = new Builder();").append(System.lineSeparator());
    for (var field : ctx.getAllFields()) {
      sb.append("    ")
          .append("bldr.")
          .append(field.name())
          .append("(")
          .append(field.name())
          .append(");")
          .append(System.lineSeparator());
    }
    sb.append("    ").append("return bldr.build();").append(System.lineSeparator());
    sb.append("  ").append("} else {").append(System.lineSeparator());
    sb.append("    ")
        .append("return (")
        .append(copyMethodRetType())
        .append(") this;")
        .append(System.lineSeparator());
    sb.append("  ").append("}").append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  private String copyMethodRetType() {
    return ctx.getProcessedClass().getClazz().getSimpleName().toString();
  }

  private List<String> parameters() {
    return ctx.getAllFields().stream()
        .map(field -> field.getSimpleTypeName() + " " + field.name())
        .toList();
  }

  /** Condition expression if one of the parameters is a different object than the field. */
  private String cond() {
    var inner =
        ctx.getAllFields().stream()
            .map(field -> "(" + field.name() + " != this." + field.name() + ")")
            .collect(Collectors.joining(" || "));
    return "(" + inner + ")";
  }
}
