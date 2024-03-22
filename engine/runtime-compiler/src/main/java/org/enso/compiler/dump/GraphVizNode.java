package org.enso.compiler.dump;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.enso.compiler.core.IR;

/**
 * Represents a node in the GraphViz graph.
 *
 * @param id Identifier of the node. Used to refer to the node in edges. Must be unique.
 * @param multiLineLabel A label in GraphViz is a simple textual attribute. To make it multi-line,
 *     we need to escape newlines with "\\n".
 * @param additionalAttrs Additional attributes to specify for the node, apart from `label`.
 * @param object The underlying object from which the node was created.
 */
record GraphVizNode(
    String id, List<String> multiLineLabel, Map<String, String> additionalAttrs, Object object) {
  public String toGraphViz() {
    var sb = new StringBuilder();
    sb.append(id);
    if (!additionalAttrs.isEmpty()) {
      sb.append(" [");
      additionalAttrs.forEach(
          (k, v) -> {
            assert Utils.hasOneLine(k) : k;
            assert Utils.hasOneLine(v) : v;
            sb.append(k);
            sb.append("=");
            if (!Utils.isSurroundedByQuotes(v)) {
              sb.append("\"").append(v).append("\"");
            } else {
              sb.append(v);
            }
            sb.append(", ");
          });
      sb.append("label=\"");
    } else {
      sb.append(" [label=\"");
    }
    for (var line : multiLineLabel) {
      sb.append(line);
      sb.append("\\n");
    }
    sb.append("\"];");
    return sb.toString();
  }

  @Override
  public int hashCode() {
    return id.hashCode();
  }

  @Override
  public boolean equals(Object otherObj) {
    if (otherObj instanceof GraphVizNode otherNode) {
      return id.equals(otherNode.id);
    }
    return false;
  }

  static class Builder {
    private String id;
    private List<String> labelLines = new ArrayList<>();
    private Map<String, String> additionalAttrs = new HashMap<>();
    private Object object;

    static Builder fromObject(Object obj) {
      var className = className(obj);
      var id = Utils.id(obj);
      var bldr = new Builder();
      bldr.object = obj;
      bldr.id = id;
      bldr.addLabelLine(id);
      bldr.addLabelLine("className: " + className);
      return bldr;
    }

    /**
     * Does not include some common info in the labels like class name, only create an empty
     * builder.
     */
    static Builder fromObjectPlain(Object obj) {
      var id = Utils.id(obj);
      var bldr = new Builder();
      bldr.object = obj;
      bldr.id = id;
      return bldr;
    }

    static Builder fromIr(IR ir) {
      var className = className(ir);
      var bldr = new Builder();
      var id = Utils.id(ir);
      bldr.object = ir;
      bldr.id = id;
      bldr.addLabelLine(id);
      bldr.addLabelLine("className: " + className);
      if (ir.location().isDefined()) {
        var loc = ir.location().get();
        bldr.addLabelLine("location_start: " + loc.start());
        bldr.addLabelLine("location_end: " + loc.end());
      } else {
        bldr.addLabelLine("location: null");
      }
      bldr.addLabelLine("id: " + ir.getId());
      bldr.addLabelLine("pass_data: " + ir.passData());
      var code = ir.showCode().replace("\n", "\\n");
      bldr.addAttribute("comment", code);
      return bldr;
    }

    Builder addLabelLine(String line) {
      labelLines.add(line);
      return this;
    }

    Builder addAttribute(String key, String value) {
      additionalAttrs.put(key, value);
      return this;
    }

    GraphVizNode build() {
      Objects.requireNonNull(id);
      Objects.requireNonNull(labelLines);
      Objects.requireNonNull(additionalAttrs);
      Objects.requireNonNull(object);
      return new GraphVizNode(id, labelLines, additionalAttrs, object);
    }

    private static String className(Object obj) {
      return Arrays.stream(obj.getClass().getName().split("\\."))
          .dropWhile(
              item ->
                  item.equals("org")
                      || item.equals("enso")
                      || item.equals("compiler")
                      || item.equals("core"))
          .collect(Collectors.joining("."));
    }
  }
}
