package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
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
  private static final Object[] NONE = new Object[0];
  private final String name;
  private final Object[] args;

  /**
   * Creates a new unresolved name.
   *
   * @param name constructor name
   * @param args arguments to apply to it
   */
  private UnresolvedConstructor(String name, Object[] args) {
    this.name = name;
    this.args = args;
  }

  /**
   * @return the scope this symbol was used in.
   */
  public String getName() {
    return name;
  }

  /**
   * Resolve in the context of a constructor.
   *
   * @return get arguments
   */
  public Object[] getArgs() {
    return args;
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
    return new UnresolvedConstructor(name, NONE);
  }

  /**
   * Marks this object as executable through the interop library.
   *
   * @return always true @ExportMessage public boolean isExecutable() { return true; }
   */
  public Object withArguments(Object[] arguments) {
    assert this.args == NONE;
    return new UnresolvedConstructor(this.name, arguments);
  }
}
