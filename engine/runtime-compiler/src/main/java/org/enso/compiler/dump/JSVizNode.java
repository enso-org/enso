package org.enso.compiler.dump;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.pass.IRPass.IRMetadata;
import org.enso.compiler.pass.resolve.DocumentationComments;

/**
 * @param object
 * @param id
 * @param level Level in the VisJS tree.
 * @param label
 * @param size
 * @param color
 * @param shape
 */
record JSVizNode(
    Object object, String id, int level, String label, Integer size, String color, String shape) {
  String toJSViz() {
    var sb = new StringBuilder();
    sb.append("{").append(System.lineSeparator());
    sb.append("  id: \"").append(id).append("\", ").append(System.lineSeparator());
    sb.append("  level: ").append(level).append(", ").append(System.lineSeparator());
    sb.append("  label: `").append(label).append("`, ").append(System.lineSeparator());
    if (size != null) {
      sb.append("size: ").append(size).append(", ").append(System.lineSeparator());
    }
    if (color != null) {
      sb.append("  color: \"").append(color).append("\", ").append(System.lineSeparator());
    }
    if (shape != null) {
      sb.append("  shape: \"").append(shape).append("\", ").append(System.lineSeparator());
    }
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  static final class Builder {
    private Object object;
    private String id;
    private int level = -1;
    private List<String> labelLines = new ArrayList<>();
    private Integer size;
    private String color;
    private String shape;
    private static final List<Class<? extends IRMetadata>> metadataToSkip =
        List.of(DocumentationComments.Doc.class);

    static Builder fromObject(Object obj) {
      var className = className(obj);
      var id = Utils.id(obj);
      var bldr = new Builder();
      bldr.object = obj;
      bldr.id = id;
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
      bldr.addLabelLine("className: " + className);
      if (ir.location().isDefined()) {
        var loc = ir.location().get();
        bldr.addLabelLine("location_start: " + loc.start());
        bldr.addLabelLine("location_end: " + loc.end());
      } else {
        bldr.addLabelLine("location: null");
      }
      bldr.addLabelLine("id: " + ir.getId());
      if (!isPassDataEmpty(ir.passData())) {
        bldr.addLabelLine("pass_data: ");
        ir.passData()
            .map(
                (pass, metadata) -> {
                  if (!metadataToSkip.contains(metadata.getClass())) {
                    var metaName = metadata.metadataName();
                    bldr.addLabelLine("  - " + metaName);
                  }
                  return null;
                });
      } else {
        bldr.addLabelLine("pass_data: []");
      }
      return bldr;
    }

    Builder object(Object object) {
      this.object = object;
      return this;
    }

    Builder id(String id) {
      this.id = id;
      return this;
    }

    Builder addLabelLine(String line) {
      labelLines.add(line);
      return this;
    }

    Builder level(int level) {
      this.level = level;
      return this;
    }

    Builder size(Integer size) {
      this.size = size;
      return this;
    }

    Builder color(String color) {
      this.color = color;
      return this;
    }

    Builder shape(String shape) {
      this.shape = shape;
      return this;
    }

    JSVizNode build() {
      Objects.requireNonNull(object);
      Objects.requireNonNull(id);
      assert !labelLines.isEmpty();
      var label = String.join(System.lineSeparator(), labelLines);
      assert level != -1 : "Level must be set";
      return new JSVizNode(object, id, level, label, size, color, shape);
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

    private static boolean isPassDataEmpty(MetadataStorage passData) {
      int[] counter = new int[] {0};
      passData.map(
          (pass, data) -> {
            counter[0]++;
            return null;
          });
      return counter[0] == 0;
    }
  }
}
