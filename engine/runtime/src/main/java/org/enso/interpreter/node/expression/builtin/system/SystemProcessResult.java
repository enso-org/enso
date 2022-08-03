package org.enso.interpreter.node.expression.builtin.system;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

import java.util.List;

@BuiltinType
public class SystemProcessResult extends Builtin {
  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("Make_System_Process_Result", "exit_code", "stdout", "stderr"));
  }
}
