package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.EnsoDate;
import org.enso.interpreter.runtime.data.EnsoDateTime;
import org.enso.interpreter.runtime.data.EnsoDuration;
import org.enso.interpreter.runtime.data.EnsoTimeOfDay;

@BuiltinMethod(
    type = "Any",
    name = "equals_builtin",
    description = "Implementation of Any.=="
)
@GenerateUncached
public abstract class EqualsNode extends Node {

  public static EqualsNode build() {
    return EqualsNodeGen.create();
  }
  public abstract boolean execute(@AcceptsError Object left, @AcceptsError Object right);

  @Specialization
  boolean equalsDates(EnsoDate left, EnsoDate right) {
    return left.equals(right);
  }

  @Specialization
  boolean equalsDateTimes(EnsoDateTime left, EnsoDateTime right) {
    return left.equals(right);
  }

  @Specialization
  boolean equalsTimeOfDay(EnsoTimeOfDay left, EnsoTimeOfDay right) {
    return left.equals(right);
  }

  @Specialization
  boolean equalsDuration(EnsoDuration leftDuration, EnsoDuration rightDuration) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Specialization
  boolean equalsAtoms(Atom leftAtom, Atom rightAtom) {
    if (leftAtom.getFields().length != rightAtom.getFields().length) {
      return false;
    }
    var equalsNodeUncached = EqualsNodeGen.getUncached();
    for (int i = 0; i < leftAtom.getFields().length; i++) {
      boolean areFieldsSame = equalsNodeUncached.execute(
          leftAtom.getFields()[i],
         rightAtom.getFields()[i]
      );
      if (!areFieldsSame) {
        return false;
      }
    }
    return true;
  }

  @Fallback
  boolean equalsGeneric(Object left, Object right) {
    return left.equals(right);
  }
}
