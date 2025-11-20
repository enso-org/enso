package org.enso.interpreter.instrument;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.polyglot.RuntimeID;

public final class ExpressionExecutionState {

  private final Map<UUID, ExecutionEnvironment> expressionConfigs;

  public ExpressionExecutionState() {
    this.expressionConfigs = new HashMap<>();
  }

  public ExpressionExecutionState(Map<UUID, ExecutionEnvironment> expressionConfigs) {
    this.expressionConfigs = expressionConfigs;
  }

  public void setExpressionConfigs(Map<UUID, ExecutionEnvironment> expressionConfigs) {
    this.expressionConfigs.putAll(expressionConfigs);
  }

  public void setExpressionExecuted(UUID expressionId) {
    expressionConfigs.remove(expressionId);
  }

  public ExecutionEnvironment getExecutionEnvironment(RuntimeID expressionId) {
    return expressionConfigs.get(expressionId);
  }
}
