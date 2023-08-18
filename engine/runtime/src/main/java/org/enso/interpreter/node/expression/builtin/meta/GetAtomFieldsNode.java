package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

@BuiltinMethod(
    type = "Meta",
    name = "get_atom_fields",
    description = "Gets the fields of an unresolved atom.",
    autoRegister = false)
public abstract class GetAtomFieldsNode extends Node {
  static GetAtomFieldsNode build() {
    return GetAtomFieldsNodeGen.create();
  }

  abstract EnsoObject execute(Atom atom);

  @Specialization
  EnsoObject doStruct(Atom atom, @CachedLibrary(limit = "2") StructsLibrary structs) {
    return ArrayLikeHelpers.wrapObjectsWithCheckAt(structs.getFields(atom));
  }
}
