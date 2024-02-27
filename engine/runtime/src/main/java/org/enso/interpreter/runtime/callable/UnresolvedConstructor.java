package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.Arrays;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.EnsoObject;

/**
 * Value representing a by-name identified constructor of a yet unknown {@link Type}. Create new
 * instance by providing name to {@link #build(String)} method. Then apply arguments to it via
 * {@link #withArguments(Object[])} method.
 *
 * <p>Let the object flow thru the interpreter and resolve it when the required {@link Type} is
 * known.
 */
@ExportLibrary(InteropLibrary.class)
public final class UnresolvedConstructor implements EnsoObject {
  private static final CallArgumentInfo[] NONE = new CallArgumentInfo[0];
  private final String name;
  private final CallArgumentInfo[] descs;
  private final Object[] args;

  /**
   * Creates a new unresolved name.
   *
   * @param name constructor name
   * @param descs arguments to apply to it
   * @param args arguments to apply to it
   */
  private UnresolvedConstructor(String name, CallArgumentInfo[] descs, Object[] args) {
    this.name = name;
    this.descs = descs;
    this.args = args;
  }

  /**
   * @return the scope this symbol was used in.
   */
  public String getName() {
    return name;
  }

  public Object[] getArgs() {
    return args;
  }

  public CallArgumentInfo[] getDescs() {
    return descs;
  }

  @Override
  public String toString() {
    return "UnresolvedName";
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return this.toString();
  }

  /**
   * Creates an instance of this node.
   *
   * @param name the name (of the constructor) we are searching for
   * @return a object representing unresolved (constructor)
   */
  public static UnresolvedConstructor build(String name) {
    return new UnresolvedConstructor(name, NONE, NONE);
  }

  /**
   * Marks this object as executable through the interop library.
   *
   * @param addionalDescriptions description of the applied arguments
   * @param additionalArguments new arguments to add to the unresolved constructor
   * @return always true @ExportMessage public boolean isExecutable() { return true; }
   */
  public Object withArguments(
      CallArgumentInfo[] addionalDescriptions, Object[] additionalArguments) {
    if (this.args == NONE) {
      return new UnresolvedConstructor(this.name, addionalDescriptions, additionalArguments);
    } else {
      var newDescs = join(this.descs, addionalDescriptions);
      var newArgs = join(this.args, additionalArguments);
      return new UnresolvedConstructor(this.name, newDescs, newArgs);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static <T> T[] join(T[] arr1, T[] arr2) {
    var ret = Arrays.copyOf(arr1, arr1.length + arr2.length);
    System.arraycopy(arr2, 0, ret, arr1.length, arr2.length);
    return ret;
  }
}
