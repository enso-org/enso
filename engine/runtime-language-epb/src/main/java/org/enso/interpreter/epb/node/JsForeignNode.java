package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;

import java.util.Arrays;

/** A node responsible for performing foreign JS calls. */
@NodeField(name = "foreignFunction", type = Object.class)
@NodeField(name = "arity", type = int.class)
@NodeField(name = "explicitSelf", type = boolean.class)
public abstract class JsForeignNode extends ForeignFunctionCallNode {

  abstract Object getForeignFunction();

  abstract int getArity();

  abstract boolean isExplicitSelf();

  /**
   * Creates a new instance of this node.
   *
   * @param argumentsCount the number of arguments the function expects (including {@code this})
   * @param jsFunction the parsed JS object (required to be {@link
   *     InteropLibrary#isExecutable(Object)})
   * @return a node able to call the JS function with given arguments
   */
  public static JsForeignNode build(Object jsFunction, int argumentsCount, boolean explicitSelf) {
    return JsForeignNodeGen.create(jsFunction, argumentsCount, explicitSelf);
  }

  @Specialization
  Object doExecute(
      Object[] arguments, @CachedLibrary("foreignFunction") InteropLibrary interopLibrary) {
    Object[] positionalArgs = new Object[getArity() - 1];
    // Implicit self is placed at the end of the parameters' list with a default value.
    // Explicit self "should" be defined as the first parameter.
    // See GenerateMethodBodies for details.
    int selfIndex = isExplicitSelf() ? 0 : getArity() - 1;
    System.arraycopy(arguments, selfIndex == 0 ? 1 : 0, positionalArgs, 0, getArity() - 1);
    try {
      return interopLibrary.invokeMember(
          getForeignFunction(), "apply", arguments[selfIndex], new ReadOnlyArray(positionalArgs));
    } catch (UnsupportedMessageException
        | UnknownIdentifierException
        | ArityException
        | UnsupportedTypeException e) {
      throw new IllegalStateException("Invalid JS function resulted from parsing.");
    }
  }
}
