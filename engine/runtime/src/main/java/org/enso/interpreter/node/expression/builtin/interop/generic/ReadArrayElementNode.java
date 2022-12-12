package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.epb.node.CoercePrimitiveNode;
import org.enso.interpreter.node.expression.foreign.CoerceNothing;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "read_array_element",
    description = "Read a value from the array specified by the index.",
    autoRegister = false)
public class ReadArrayElementNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);

  private @Child CoercePrimitiveNode coercion = CoercePrimitiveNode.build();
  private @Child CoerceNothing nothingCoercion = CoerceNothing.build();
  private final BranchProfile err = BranchProfile.create();

  Object execute(Object array, long index) {
    try {
      return nothingCoercion.execute(coercion.execute(library.readArrayElement(array, index)));
    } catch (UnsupportedMessageException e) {
      err.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError(builtins.array(), array, "array"), this);
    } catch (InvalidArrayIndexException e) {
      err.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(builtins.error().makeInvalidArrayIndex(array, index), this);
    }
  }
}
