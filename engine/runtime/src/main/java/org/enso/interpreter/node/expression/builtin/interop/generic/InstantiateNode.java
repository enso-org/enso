package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "new",
    description = "Instantiates a polyglot constructor.")
public abstract class InstantiateNode extends Node {

  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private final BranchProfile err = BranchProfile.create();

  static InstantiateNode build() {
    return InstantiateNodeGen.create();
  }

  abstract Object execute(Object constructor, Object arguments);

  @Specialization
  Object doExecute(
      Object constructor, Object arguments, @Cached("build()") CoerceArrayNode coerce) {
    try {
      return library.instantiate(constructor, coerce.execute(arguments));
    } catch (UnsupportedMessageException | ArityException | UnsupportedTypeException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
