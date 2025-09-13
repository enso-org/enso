package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.util.ArrayList;
import java.util.function.BiConsumer;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

@BuiltinMethod(type = "Meta", name = "get_argument_names", autoRegister = false)
public abstract class GetArgumentNamesNode extends Node {
  static GetArgumentNamesNode build() {
    return GetArgumentNamesNodeGen.create();
  }

  abstract Object execute(
      Object obj, boolean pending, boolean defaulted, boolean preapplied, boolean oversaturated);

  @Specialization
  final Object fieldNamesForAtomCtor(
      AtomConstructor atomConstructor,
      boolean pending,
      boolean defaulted,
      boolean preapplied,
      boolean oversaturated) {
    var withCheck = FindAtomConstructorNode.findAtomConstructor(this, atomConstructor, null);
    if (withCheck == atomConstructor) {
      return namesFor(
          atomConstructor.getConstructorFunction(), pending, defaulted, preapplied, oversaturated);
    } else {
      return withCheck;
    }
  }

  @Specialization
  final Object argumentNamesForFn(
      Function fn, boolean pending, boolean defaulted, boolean preapplied, boolean oversaturated) {
    return namesFor(fn, pending, defaulted, preapplied, oversaturated);
  }

  @Fallback
  final EnsoObject fieldNamesForAny(
      Object any, boolean pending, boolean defaulted, boolean preapplied, boolean oversaturated) {
    return ArrayLikeHelpers.asVectorEmpty();
  }

  @CompilerDirectives.TruffleBoundary
  private static final EnsoObject namesFor(
      Function fn, boolean pending, boolean defaulted, boolean preapplied, boolean oversaturated) {
    var names = new ArrayList<String>();
    BiConsumer<String, Object> addName = (name, __) -> names.add(name);
    fn.iterateArguments(
        pending ? names::add : null,
        defaulted ? names::add : null,
        preapplied ? addName : null,
        oversaturated ? addName : null);
    var result = new Text[names.size()];
    for (var i = 0; i < result.length; i++) {
      result[i] = Text.create(names.get(i));
    }
    return ArrayLikeHelpers.asVectorEnsoObjects(result);
  }
}
