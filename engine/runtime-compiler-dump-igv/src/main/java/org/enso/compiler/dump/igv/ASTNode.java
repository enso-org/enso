package org.enso.compiler.dump.igv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.dump.service.IRSource;

final class ASTNode {

  private final List<ASTEdge> edges = new ArrayList<>();
  private final int id;
  private final Object object;
  // Frozen
  private final Map<String, Object> properties;
  private final ASTNodeClass nodeClass;
  private final List<ASTNode> children = new ArrayList<>();
  // May be null;
  private final ASTLocation location;

  ASTNode(int id, Object object, Map<String, Object> props, ASTLocation location) {
    this.id = id;
    this.object = object;
    props.put("nodeSourcePosition", location);
    this.properties = props;
    this.nodeClass = new ASTNodeClass(this);
    this.location = location;
    if (object instanceof IR && location == null) {
      throw new IllegalArgumentException("IR object must have location");
    }
  }

  public Object getObject() {
    return object;
  }

  public int getId() {
    return id;
  }

  public List<ASTEdge> getEdges() {
    return edges;
  }

  public ASTNodeClass getNodeClass() {
    return nodeClass;
  }

  public Map<String, Object> getProperties() {
    return properties;
  }

  public ASTLocation getLocation() {
    return location;
  }

  public void addChild(ASTNode child) {
    children.add(child);
  }

  public void addEdge(ASTEdge edge) {
    edges.add(edge);
  }

  static final class Builder {
    private int id = -1;
    private Object object;
    private final Map<String, Object> properties = new LinkedHashMap<>();
    private ASTLocation location;

    public static Builder fromIr(IR ir, IRSource<? extends IR> ctx) {
      var bldr = new Builder();
      var label = Utils.label(ir);
      var location = ASTLocation.fromIdentifiedLocation(ir.identifiedLocation(), ctx);
      bldr.object = ir;
      bldr.location = location;
      bldr.property("label", label);
      bldr.property("IRClassName", ir.getClass().getName());
      bldr.property("location", ir.identifiedLocation());
      bldr.property("passData", ir.passData());
      bldr.property("uuid", ir.getId());
      return bldr;
    }

    public Builder id(int id) {
      this.id = id;
      return this;
    }

    public Builder property(String key, Object value) {
      properties.put(key, value);
      return this;
    }

    public Builder location(ASTLocation location) {
      this.location = location;
      return this;
    }

    public ASTNode build() {
      if (id == -1) {
        throw new IllegalArgumentException("ID must be set");
      }
      if (object == null) {
        throw new IllegalArgumentException("IR must be set");
      }
      return new ASTNode(id, object, properties, location);
    }

    private static String simpleClassName(Object obj) {
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
