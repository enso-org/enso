package org.enso.compiler.dump.igv;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.enso.compiler.dump.igv.ASTEdge.EdgeType;
import org.graalvm.graphio.GraphBlocks;
import org.graalvm.graphio.GraphStructure;

final class ASTDumpStructure
    implements GraphStructure<EnsoAST, ASTNode, ASTNodeClass, List<ASTEdge>>,
        GraphBlocks<EnsoAST, ASTBlock, ASTNode> {

  @Override
  public EnsoAST graph(EnsoAST currentGraph, Object obj) {
    if (obj instanceof EnsoAST ensoAST) {
      return ensoAST;
    }
    return null;
  }

  @Override
  public Iterable<? extends ASTNode> nodes(EnsoAST graph) {
    return graph.getNodes();
  }

  @Override
  public int nodesCount(EnsoAST graph) {
    return graph.getNodes().size();
  }

  @Override
  public int nodeId(ASTNode node) {
    return node.getId();
  }

  @Override
  public boolean nodeHasPredecessor(ASTNode node) {
    return false;
  }

  @Override
  public void nodeProperties(EnsoAST graph, ASTNode node, Map<String, ? super Object> properties) {
    properties.putAll(node.getProperties());
  }

  @Override
  public ASTNode node(Object obj) {
    if (obj instanceof ASTNode astNode) {
      return astNode;
    }
    return null;
  }

  @Override
  public ASTNodeClass nodeClass(Object obj) {
    if (obj instanceof ASTNodeClass astNodeClass) {
      return astNodeClass;
    }
    return null;
  }

  @Override
  public ASTNodeClass classForNode(ASTNode node) {
    return node.getNodeClass();
  }

  @Override
  public String nameTemplate(ASTNodeClass nodeClass) {
    return "{p#label}";
  }

  @Override
  public Object nodeClassType(ASTNodeClass nodeClass) {
    return nodeClass.getClass();
  }

  @Override
  public List<ASTEdge> portInputs(ASTNodeClass nodeClass) {
    return List.of();
  }

  @Override
  public List<ASTEdge> portOutputs(ASTNodeClass nodeClass) {
    return nodeClass.node().getEdges();
  }

  @Override
  public int portSize(List<ASTEdge> port) {
    return port.size();
  }

  @Override
  public boolean edgeDirect(List<ASTEdge> port, int index) {
    return true;
  }

  @Override
  public String edgeName(List<ASTEdge> port, int index) {
    return port.get(index).label();
  }

  @Override
  public Object edgeType(List<ASTEdge> port, int index) {
    return EdgeType.EDGE_TYPE;
  }

  @Override
  public Collection<? extends ASTNode> edgeNodes(
      EnsoAST graph, ASTNode node, List<ASTEdge> port, int index) {
    return List.of(port.get(index).node());
  }

  @Override
  public Collection<? extends ASTBlock> blocks(EnsoAST graph) {
    return graph.getBlocks();
  }

  @Override
  public int blockId(ASTBlock block) {
    return block.getId();
  }

  @Override
  public Collection<? extends ASTNode> blockNodes(EnsoAST info, ASTBlock block) {
    return block.getNodes();
  }

  @Override
  public Collection<? extends ASTBlock> blockSuccessors(ASTBlock block) {
    return block.getSuccessors();
  }
}
