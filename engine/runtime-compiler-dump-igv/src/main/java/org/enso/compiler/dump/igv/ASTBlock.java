package org.enso.compiler.dump.igv;

import java.util.ArrayList;
import java.util.List;

final class ASTBlock {
  private final int id;
  private final List<ASTNode> nodes;
  private final List<ASTBlock> successors;

  ASTBlock(int id, List<ASTNode> nodes, List<ASTBlock> successors) {
    this.id = id;
    this.nodes = nodes;
    this.successors = successors;
  }

  public int getId() {
    return id;
  }

  public List<ASTNode> getNodes() {
    return nodes;
  }

  public List<ASTBlock> getSuccessors() {
    return successors;
  }

  static final class Builder {
    private int id;
    private final List<ASTNode> nodes = new ArrayList<>();
    private final List<ASTBlock> successors = new ArrayList<>();

    static Builder fromId(int id) {
      var bldr = new Builder();
      bldr.id = id;
      return bldr;
    }

    Builder addNode(ASTNode node) {
      nodes.add(node);
      return this;
    }

    void addSuccessor(ASTBlock successor) {
      successors.add(successor);
    }

    ASTBlock build() {
      if (nodes.isEmpty()) {
        throw new IllegalArgumentException("Cannot build an empty block");
      }
      return new ASTBlock(id, nodes, successors);
    }
  }
}
