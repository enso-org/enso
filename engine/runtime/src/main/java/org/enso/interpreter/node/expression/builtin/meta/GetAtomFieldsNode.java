package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.StructsLibrary;
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

  @Specialization(
      limit = "3",
      guards = {"atom.getConstructor() == cons"})
  @ExplodeLoop
  EnsoObject doStruct(
      Atom atom,
      @Cached("atom.getConstructor()") AtomConstructor cons,
      @CachedLibrary(limit = "2") StructsLibrary structs) {
    var arr = new Object[cons.getArity()];
    for (var i = 0; i < arr.length; i++) {
      arr[i] = structs.getField(atom, i);
    }
    return ArrayLikeHelpers.wrapObjectsWithCheckAt(arr);
  }
}
