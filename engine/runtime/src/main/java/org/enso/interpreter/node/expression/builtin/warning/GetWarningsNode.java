package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;

import java.util.Arrays;
import java.util.Comparator;

@BuiltinMethod(
    type = "Prim_Warning",
    name = "get_all",
    description = "Gets all the warnings associated with the value.")
public abstract class GetWarningsNode extends Node {
  abstract Array execute(Object _this, @AcceptsWarning Object value);

  static GetWarningsNode build() {
    return GetWarningsNodeGen.create();
  }

  @Specialization
  Array doWarning(Object _this, WithWarnings value) {
    Warning[] warnings = value.getWarningsArray();
    Arrays.sort(warnings, Comparator.comparing(Warning::getCreationTime).reversed());
    Object[] result = new Object[warnings.length];
    System.arraycopy(warnings, 0, result, 0, warnings.length);
    return new Array(result);
  }

  @Fallback
  Array doOther(Object _this, Object value) {
    return new Array();
  }
}
