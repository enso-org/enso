package org.enso.interpreter.node.expression.builtin.error;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

@BuiltinType(name = "Standard.Base.Errors.Problem_Behavior.Problem_Behavior")
public class ProblemBehavior extends Builtin {

  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("Ignore"), new Cons("Report_Warning"), new Cons("Report_Error"));
  }

  public AtomConstructor getIgnore() {
    return getConstructors()[0];
  }

  public AtomConstructor getReportWarning() {
    return getConstructors()[1];
  }

  public AtomConstructor getReportError() {
    return getConstructors()[2];
  }
}
