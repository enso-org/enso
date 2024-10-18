package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;

@NodeInfo(
    shortName = "get_field_with_match",
    description = "Finds an atom field in multiple constructors")
final class GetFieldWithMatchNode extends GetFieldBaseNode {

  record GetterPair(AtomConstructor target, int index) {}

  private final @CompilerDirectives.CompilationFinal(dimensions = 1) GetterPair[] getterPairs;
  private @Children StructsLibrary[] structsLibraries;

  public GetFieldWithMatchNode(
      EnsoLanguage language,
      String name,
      Type type,
      ModuleScope moduleScope,
      GetterPair[] getterPairs) {
    super(language, type, name, moduleScope);
    this.getterPairs = getterPairs;
    this.structsLibraries = new StructsLibrary[getterPairs.length];
    for (int i = 0; i < getterPairs.length; i++) {
      this.structsLibraries[i] = StructsLibrary.getFactory().createDispatched(3);
    }
  }

  @ExplodeLoop
  public Object execute(VirtualFrame frame) {
    // this is safe, as only Atoms will ever get here through method dispatch.
    Atom atom = (Atom) Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    var constructor = atom.getConstructor();
    for (int i = 0; i < getterPairs.length; i++) {
      var getter = getterPairs[i];
      if (getter.target == constructor) {
        return structsLibraries[i].getField(atom, getter.index);
      }
    }
    throw noSuchFieldPanic(atom);
  }
}
