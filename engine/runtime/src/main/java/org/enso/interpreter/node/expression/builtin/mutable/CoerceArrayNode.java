package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

public abstract class CoerceArrayNode extends Node {
  private @Child InteropLibrary library = InteropLibrary.getFactory().createDispatched(10);

  public static CoerceArrayNode build() {
    return CoerceArrayNodeGen.create();
  }

  public abstract Object[] execute(Object value);

  @Specialization
  Object[] doArray(Array arr) {
    return arr.getItems();
  }

  @Specialization
  Object[] doVector(Vector arr, @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    try {
      return convertToArray(arr, hostValueToEnsoNode);
    } catch (UnsupportedMessageException e) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      Atom err = builtins.error().makeTypeError(builtins.array(), arr, "arr");
      throw new PanicException(err, this);
    } catch (InvalidArrayIndexException e) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeInvalidArrayIndex(arr, e.getInvalidIndex()), this);
    }
  }

  @Specialization(guards = "interop.hasArrayElements(arr)")
  Object[] doArrayLike(
      Object arr,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    try {
      return convertToArray(arr, hostValueToEnsoNode);
    } catch (UnsupportedMessageException e) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      Atom err = builtins.error().makeTypeError(builtins.array(), arr, "arr");
      throw new PanicException(err, this);
    } catch (InvalidArrayIndexException e) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeInvalidArrayIndex(arr, e.getInvalidIndex()), this);
    }
  }

  private Object[] convertToArray(Object arr, HostValueToEnsoNode hostValueToEnsoNode)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    int argsLength = (int) library.getArraySize(arr);
    Object[] arr1 = new Object[argsLength];
    for (int i = 0; i < argsLength; i++) {
      arr1[i] = hostValueToEnsoNode.execute(library.readArrayElement(arr, i));
    }
    return arr1;
  }

  @Fallback
  Object[] doOther(Object arr) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    Atom error = builtins.error().makeTypeError("array", arr, "arr");
    throw new PanicException(error, this);
  }
}
