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
    name = "get_reassignments",
    description = "Gets the list of locations where the warnings was reassigned.")
public class GetReassignmentsNode extends Node {
  Array execute(Object _this, Warning warning) {
    Warning.Reassignment[] reassignments =
        warning.getReassignments().toArray(Warning.Reassignment[]::new);
    return new Array(Arrays.copyOf(reassignments, reassignments.length, Object[].class));
  }
}
