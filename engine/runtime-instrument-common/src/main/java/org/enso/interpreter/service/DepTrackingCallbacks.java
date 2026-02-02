package org.enso.interpreter.service;

import org.enso.interpreter.runtime.execution.Ref;
import org.enso.interpreter.runtime.execution.RuntimeAnalysis;
import org.enso.polyglot.DepTrackingService;
import org.enso.polyglot.RuntimeID;

public class DepTrackingCallbacks implements DepTrackingService.Callbacks {

  private final RuntimeAnalysis runtimeAnalysis;

  public DepTrackingCallbacks(RuntimeAnalysis runtimeAnalysis) {
    this.runtimeAnalysis = runtimeAnalysis;
  }

  @Override
  public void startVariableAssignment(RuntimeID runtimeID) {
    runtimeAnalysis.startRhsExecution(runtimeID);
  }

  @Override
  public void endVariableAssignment(RuntimeID runtimeID) {
    runtimeAnalysis.endRhsExecution(runtimeID);
  }

  @Override
  public Object registerReturnValue(RuntimeID runtimeID, Object result) {
    if (result instanceof Ref r) {
      var currentRhs = runtimeAnalysis.currentRhs();
      if (currentRhs != null) {
        r.registerDependency(currentRhs);
      }
      return r.get();
    }
    return result;
  }

  @Override
  public Object wrapAsReference(RuntimeID runtimeID, Object value) {
    assert (!(value instanceof Ref));
    var r = runtimeAnalysis.get(runtimeID);
    r.update(value);
    return r;
  }
}
