package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.data.Type;

@BuiltinMethod(
    type = "Meta",
    name = "get_atom_constructor",
    description = "Gets the constructor of an atom.",
    autoRegister = false)
public abstract class GetAtomConstructorNode extends Node {
  abstract Object execute(Object atom);

  static GetAtomConstructorNode build() {
    return GetAtomConstructorNodeGen.create();
  }

  @Specialization
  Object doAtom(Atom atom, @CachedLibrary(limit = "10") StructsLibrary structs) {
    return structs.getConstructor(atom);
  }

  @Specialization
  Object doType(Type type) {
    return type;
  }
}
