package org.enso.compiler.dump.igv;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.enso.compiler.dump.igv.ASTEdge.EdgeType;
import org.graalvm.graphio.GraphBlocks;
import org.graalvm.graphio.GraphElements;
import org.graalvm.graphio.GraphLocations;
import org.graalvm.graphio.GraphStructure;

final class ASTDumpStructure
    implements GraphStructure<EnsoModuleAST, ASTNode, ASTNodeClass, List<ASTEdge>>,
        GraphBlocks<EnsoModuleAST, ASTBlock, ASTNode>,
        GraphElements<ASTMethod, Object, ASTMethod.Signature, ASTLocation>,
        GraphLocations<ASTMethod, ASTLocation, ASTLocation> {

  @Override
  public EnsoModuleAST graph(EnsoModuleAST currentGraph, Object obj) {
    if (obj instanceof EnsoModuleAST ensoAST) {
      return ensoAST;
    }
    return null;
  }

  @Override
  public Iterable<? extends ASTNode> nodes(EnsoModuleAST graph) {
    return graph.getNodes();
  }

  @Override
  public int nodesCount(EnsoModuleAST graph) {
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
  public void nodeProperties(
      EnsoModuleAST graph, ASTNode node, Map<String, ? super Object> properties) {
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
      EnsoModuleAST graph, ASTNode node, List<ASTEdge> port, int index) {
    return List.of(port.get(index).node());
  }

  @Override
  public Collection<? extends ASTBlock> blocks(EnsoModuleAST graph) {
    return graph.getBlocks();
  }

  @Override
  public int blockId(ASTBlock block) {
    return block.getId();
  }

  @Override
  public Collection<? extends ASTNode> blockNodes(EnsoModuleAST info, ASTBlock block) {
    return block.getNodes();
  }

  @Override
  public Collection<? extends ASTBlock> blockSuccessors(ASTBlock block) {
    return block.getSuccessors();
  }

  @Override
  public ASTMethod method(Object obj) {
    if (obj instanceof ASTMethod m) {
      return m;
    }
    return null;
  }

  @Override
  public byte[] methodCode(ASTMethod method) {
    return new byte[0];
  }

  @Override
  public int methodModifiers(ASTMethod method) {
    return 0;
  }

  @Override
  public ASTMethod.Signature methodSignature(ASTMethod method) {
    return ASTMethod.Signature.NONE;
  }

  @Override
  public String methodName(ASTMethod method) {
    return method.getName();
  }

  @Override
  public Object methodDeclaringClass(ASTMethod method) {
    return ASTMethod.class;
  }

  @Override
  public Object field(Object object) {
    return null;
  }

  @Override
  public int fieldModifiers(Object field) {
    return 0;
  }

  @Override
  public String fieldTypeName(Object field) {
    return null;
  }

  @Override
  public String fieldName(Object field) {
    return null;
  }

  @Override
  public Object fieldDeclaringClass(Object field) {
    return null;
  }

  @Override
  public ASTMethod.Signature signature(Object object) {
    return object instanceof ASTMethod.Signature s ? s : null;
  }

  @Override
  public int signatureParameterCount(ASTMethod.Signature signature) {
    return 0;
  }

  @Override
  public String signatureParameterTypeName(ASTMethod.Signature signature, int index) {
    return null;
  }

  @Override
  public String signatureReturnTypeName(ASTMethod.Signature signature) {
    return null;
  }

  @Override
  public ASTLocation nodeSourcePosition(Object object) {
    if (object instanceof ASTLocation location) {
      return location;
    }
    if (object instanceof ASTNode node) {
      return node.getLocation();
    }
    return null;
  }

  @Override
  public ASTMethod nodeSourcePositionMethod(ASTLocation pos) {
    return ASTMethod.UNKNOWN;
  }

  @Override
  public ASTLocation nodeSourcePositionCaller(ASTLocation pos) {
    return null;
  }

  @Override
  public int nodeSourcePositionBCI(ASTLocation pos) {
    return 0;
  }

  @Override
  public StackTraceElement methodStackTraceElement(ASTMethod method, int bci, ASTLocation pos) {
    return null;
  }

  @Override
  public Iterable<ASTLocation> methodLocation(ASTMethod method, int bci, ASTLocation pos) {
    return List.of(pos);
  }

  @Override
  public String locationLanguage(ASTLocation location) {
    return "enso";
  }

  @Override
  public URI locationURI(ASTLocation location) {
    return location.getLocationUri();
  }

  @Override
  public int locationLineNumber(ASTLocation location) {
    return location.getLineNum();
  }

  @Override
  public int locationOffsetStart(ASTLocation location) {
    return location.getOffsetStart();
  }

  @Override
  public int locationOffsetEnd(ASTLocation location) {
    return location.getOffsetEnd();
  }
}
