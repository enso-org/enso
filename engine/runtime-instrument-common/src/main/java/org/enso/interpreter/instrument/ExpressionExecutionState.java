package org.enso.interpreter.instrument;

import org.enso.interpreter.runtime.state.ExecutionEnvironment;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

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

  public ExecutionEnvironment getExpressionExecutionEnvironment(UUID expressionId) {
    return expressionConfigs.get(expressionId);
  }
}
