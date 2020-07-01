package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.ObjectType;

public class StateObjectType extends ObjectType {
  public static StateObjectType SINGLETON = new StateObjectType();

  @Override
  public Class<?> dispatch() {
    return StateObjectType.class;
  }
}
