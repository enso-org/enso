package org.enso.interpreter.node.expression.builtin.unsafe;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.struct.Struct;

@BuiltinMethod(
    type = "Unsafe",
    name = "set_atom_field",
    description = "Unsafely, in place, sets the value of an atom field by index.",
    autoRegister = false)
public class SetAtomFieldNode extends Node {
  Struct execute(Struct struct, long index, Object value) {
    //    struct.getFields()[(int) index] = value;
    return struct;
  }
}
