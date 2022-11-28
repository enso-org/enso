package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "invoke",
    description = "Invokes a polyglot method by name, dispatching by the target argument.",
    autoRegister = false)
public abstract class InvokeNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();
  private final BranchProfile err = BranchProfile.create();

  static InvokeNode build() {
    return InvokeNodeGen.create();
  }

  abstract Object execute(Object target, Object name, Object arguments);

  @Specialization
  Object doExecute(
      Object target, Object name, Object arguments, @Cached("build()") CoerceArrayNode coerce) {
    try {
      return library.invokeMember(
          target, expectStringNode.execute(name), coerce.execute(arguments));
    } catch (UnsupportedMessageException
        | ArityException
        | UnsupportedTypeException
        | UnknownIdentifierException e) {
      err.enter();
      throw new PanicException(e.getMessage(), this);
    }
  }
}
