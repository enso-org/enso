package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;

/** A node responsible for performing foreign JS calls. */
@NodeField(name = "foreignFunction", type = Object.class)
@NodeField(name = "arity", type = int.class)
public abstract class JsForeignNode extends ForeignFunctionCallNode {

  abstract int getArity();

  abstract Object getForeignFunction();

  /**
   * Creates a new instance of this node.
   *
   * @param argumentsCount the number of arguments the function expects (including {@code this})
   * @param jsFunction the parsed JS object (required to be {@link
   *     InteropLibrary#isExecutable(Object)})
   * @return a node able to call the JS function with given arguments
   */
  public static JsForeignNode build(int argumentsCount, Object jsFunction) {
    return JsForeignNodeGen.create(jsFunction, argumentsCount);
  }

  @Specialization
  Object doExecute(
      Object[] arguments, @CachedLibrary("foreignFunction") InteropLibrary interopLibrary) {
    Object[] positionalArgs = new Object[getArity() - 1];
    if (getArity() - 1 >= 0) System.arraycopy(arguments, 1, positionalArgs, 0, getArity() - 1);
    try {
      return interopLibrary.invokeMember(
          getForeignFunction(), "apply", arguments[0], new ReadOnlyArray(positionalArgs));
    } catch (UnsupportedMessageException
        | UnknownIdentifierException
        | ArityException
        | UnsupportedTypeException e) {
      throw new IllegalStateException("Invalid JS function resulted from parsing.");
    }
  }
}
