package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;

/** A node responsible for performing foreign JS calls. */
@NodeField(name = "foreignFunction", type = Object.class)
@NodeField(name = "arity", type = int.class)
public abstract class JsForeignNode extends ForeignFunctionCallNode {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();

  abstract Object getForeignFunction();

  abstract int getArity();

  /**
   * Creates a new instance of this node.
   *
   * @param argumentsCount the number of arguments the function expects (including {@code this})
   * @param jsFunction the parsed JS object (required to be {@link
   *     InteropLibrary#isExecutable(Object)})
   * @return a node able to call the JS function with given arguments
   */
  public static JsForeignNode build(Object jsFunction, int argumentsCount) {
    return JsForeignNodeGen.create(jsFunction, argumentsCount);
  }

  @Specialization
  Object doExecute(
      Object[] arguments, @CachedLibrary("foreignFunction") InteropLibrary interopLibrary)
      throws InteropException {
    int newLength = getArity() - 1;
    Object[] positionalArgs = new Object[newLength];
    System.arraycopy(arguments, 1, positionalArgs, 0, newLength);
    return coercePrimitiveNode.execute(
        interopLibrary.invokeMember(
            getForeignFunction(), "apply", arguments[0], new ReadOnlyArray(positionalArgs)));
  }
}
