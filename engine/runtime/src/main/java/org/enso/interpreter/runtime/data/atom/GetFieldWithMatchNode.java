package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
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

  @Override
  public Object execute(VirtualFrame frame) {
    // this is safe, as only Atoms will ever get here through method dispatch.
    var self = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[0];
    if (self instanceof Atom atom) {
      var value = searchAtom(atom);
      if (value != null) {
        return value;
      } else {
        throw noSuchFieldPanic(atom);
      }
    } else if (self instanceof EnsoMultiValue emv) {
      CompilerDirectives.transferToInterpreter();
      var types = TypeOfNode.getUncached().findAllTypesOrNull(self, false);
      if (types != null) {
        Atom firstAtom = null;
        for (var t : types) {
          var v = EnsoMultiValue.CastToNode.getUncached().findTypeOrNull(t, emv, false, false);
          if (v instanceof Atom atom) {
            if (firstAtom == null) {
              firstAtom = atom;
            }
            var value = searchAtom(atom);
            if (value != null) {
              return value;
            }
          }
        }
        if (firstAtom != null) {
          throw noSuchFieldPanic(firstAtom);
        }
      }
    }
    var ctx = EnsoContext.get(this);
    throw ctx.raiseAssertionPanic(this, fieldName, null);
  }

  @ExplodeLoop
  private Object searchAtom(Atom atom) {
    var constructor = atom.getConstructor();
    for (int i = 0; i < getterPairs.length; i++) {
      var getter = getterPairs[i];
      if (getter.target == constructor) {
        return structsLibraries[i].getField(atom, getter.index);
      }
    }
    return null;
  }
}
