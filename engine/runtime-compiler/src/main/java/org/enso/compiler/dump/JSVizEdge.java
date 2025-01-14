package org.enso.compiler.dump;

import java.util.Objects;

final class JSVizEdge {
  private final String fromId;
  private final String toId;
  private final String label;
  private final String color;
  private final boolean dashes;

  JSVizEdge(String fromId, String toId, String label, String color, boolean dashes) {
    this.fromId = fromId;
    this.toId = toId;
    this.label = label;
    this.color = color;
    this.dashes = dashes;
  }

  String toJSViz() {
    var sb = new StringBuilder();
    sb.append("{").append(System.lineSeparator());
    sb.append("  from: \"").append(fromId).append("\", ").append(System.lineSeparator());
    sb.append("  to: \"").append(toId).append("\", ").append(System.lineSeparator());
    if (label != null) {
      sb.append("  label: `").append(label).append("`, ").append(System.lineSeparator());
    }
    if (color != null) {
      sb.append("  color: \"").append(color).append("\", ").append(System.lineSeparator());
    }
    if (dashes) {
      sb.append("  dashes: true, ").append(System.lineSeparator());
    }
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  static final class Builder {
    private String fromId;
    private String toId;
    private String label;
    private String color;
    private boolean dashes = false;

    Builder fromId(String fromId) {
      this.fromId = fromId;
      return this;
    }

    Builder toId(String toId) {
      this.toId = toId;
      return this;
    }

    Builder label(String label) {
      this.label = label;
      return this;
    }

    Builder dashes(boolean dashes) {
      this.dashes = dashes;
      return this;
    }

    Builder color(String color) {
      this.color = color;
      return this;
    }

    JSVizEdge build() {
      Objects.requireNonNull(fromId);
      Objects.requireNonNull(toId);
      return new JSVizEdge(fromId, toId, label, color, dashes);
    }
  }
}
