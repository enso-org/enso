package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Polyglot",
    name = "write_array_element",
    description = "Write a value from the array specified by the index.",
    autoRegister = false)
public class WriteArrayElementNode extends Node {
  private @Child InteropLibrary library =
      InteropLibrary.getFactory().createDispatched(Constants.CacheSizes.BUILTIN_INTEROP_DISPATCH);

  private final BranchProfile err = BranchProfile.create();

  Object execute(Object array, long index, Object value) {
    try {
      library.writeArrayElement(array, index, value);
      return EnsoContext.get(this).getBuiltins().nothing();
    } catch (UnsupportedMessageException e) {
      err.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError(builtins.array(), array, "array"), this);
    } catch (InvalidArrayIndexException e) {
      err.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(builtins.error().makeInvalidArrayIndex(array, index), this);
    } catch (UnsupportedTypeException e) {
      err.enter();
      throw EnsoContext.get(library).raiseAssertionPanic(library, null, e);
    }
  }
}
