package org.enso.interpreter.test;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.UUID;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.test.instruments.service.FunctionCallInfo;
import org.enso.interpreter.test.instruments.service.RuntimeTestService;
import org.openide.util.lookup.ServiceProvider;

@ServiceProvider(service = RuntimeTestService.class)
public class RuntimeTestServiceImpl implements RuntimeTestService {
  @Override
  public UUID getNodeID(Node node) {
    if (node instanceof ExpressionNode exprNode) {
      return exprNode.getId();
    } else if (node instanceof FunctionCallInstrumentationNode funcNode) {
      return funcNode.getId();
    }
    return null;
  }

  @Override
  public boolean isExpressionNode(Object node) {
    return node instanceof ExpressionNode;
  }

  @Override
  public boolean isTailCallException(Object obj) {
    return obj instanceof TailCallException;
  }

  @Override
  public boolean isFunctionCallInstrumentationNode(Object node) {
    return node instanceof FunctionCallInstrumentationNode;
  }

  @Override
  public boolean isFunctionCall(Object obj) {
    return obj instanceof FunctionCall;
  }

  @Override
  public FunctionCallInfo extractFunctionCallInfo(Object functionCallObj) {
    if (functionCallObj instanceof FunctionCall functionCall) {
      RootNode rootNode = functionCall.getFunction().getCallTarget().getRootNode();
      if (rootNode instanceof MethodRootNode methodNode) {
        String moduleName = methodNode.getModuleScope().getModule().getName().toString();
        String typeName = methodNode.getType().getQualifiedName().toString();
        String functionName = methodNode.getMethodName();
        return new FunctionCallInfo(moduleName, typeName, functionName);
      } else {
        return new FunctionCallInfo(null, null, rootNode.getName());
      }
    }
    return null;
  }
}
