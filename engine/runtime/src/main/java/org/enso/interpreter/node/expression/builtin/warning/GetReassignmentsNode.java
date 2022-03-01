package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.Warning;

import java.util.Arrays;
import java.util.Comparator;

@BuiltinMethod(
    type = "Prim_Warning",
    name = "get_value",
    description = "Gets the payload of the warning.")
public class GetReassignmentsNode extends Node {
  Array execute(Object _this, Warning warning) {
    Warning.Reassignment[] reassignments =
        warning.getReassignments().toArray(Warning.Reassignment[]::new);
    Arrays.sort(reassignments, Comparator.comparing(Warning.Reassignment::getTime));
    Object[] result = new Object[reassignments.length];
    TruffleLanguage.Env env = Context.get(this).getEnvironment();
    for (int i = 0; i < result.length; i++) {
      result[i] = env.asGuestValue(reassignments[i]);
    }
    return new Array(result);
  }
}
