package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.runtime.data.Array;

/** A node responsible for performing foreign JS calls. */
public abstract class JsForeignNode extends ForeignFunctionCallNode {

  final Object jsFun;
  private final int argsCount;

  JsForeignNode(int argsCount, Object jsFun) {
    this.argsCount = argsCount;
    this.jsFun = jsFun;
  }

  /**
   * Creates a new instance of this node.
   *
   * @param argumentsCount the number of arguments the function expects (including {@code this})
   * @param jsFunction the parsed JS object (required to be {@link
   *     InteropLibrary#isExecutable(Object)})
   * @return a node able to call the JS function with given arguments
   */
  public static JsForeignNode build(int argumentsCount, Object jsFunction) {
    return JsForeignNodeGen.create(argumentsCount, jsFunction);
  }

  @Specialization
  Object doExecute(Object[] arguments, @CachedLibrary("jsFun") InteropLibrary interopLibrary) {
    Object[] positionalArgs = new Object[argsCount - 1];
    if (argsCount - 1 >= 0) System.arraycopy(arguments, 1, positionalArgs, 0, argsCount - 1);
    try {
      return interopLibrary.invokeMember(jsFun, "apply", arguments[0], new Array(positionalArgs));
    } catch (UnsupportedMessageException
        | UnknownIdentifierException
        | ArityException
        | UnsupportedTypeException e) {
      throw new IllegalStateException("Invalid JS function resulted from parsing.");
    }
  }
}
