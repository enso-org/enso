package org.enso.interpreter.test.instruments.service;

import com.oracle.truffle.api.nodes.Node;
import java.util.UUID;

/**
 * A service that provides information from the `runtime` project to the instruments in this project
 * (`runtime-test-instruments`). Note that this project cannot have a compile time dependency on
 * `runtime`, thus, we need to use a service provider mechanism.
 */
public interface RuntimeTestService {
  UUID getNodeID(Node node);

  boolean isExpressionNode(Object node);

  boolean isTailCallException(Object obj);

  boolean isFunctionCallInstrumentationNode(Object node);

  boolean isFunctionCall(Object obj);

  FunctionCallInfo extractFunctionCallInfo(Object functionCall);
}
