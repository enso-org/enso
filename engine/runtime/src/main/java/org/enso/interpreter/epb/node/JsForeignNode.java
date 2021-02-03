package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.epb.EpbContext;
import org.enso.interpreter.epb.EpbLanguage;
import org.enso.interpreter.runtime.data.Array;

public abstract class JsForeignNode extends ForeignFunctionCallNode {

  final Object jsFun;
  final int argsCount;

  JsForeignNode(int argsCount, Object jsFun) {
    this.argsCount = argsCount;
    this.jsFun = jsFun;
  }

  @Specialization
  Object doExecute(Object[] arguments, @CachedLibrary("jsFun") InteropLibrary interopLibrary) {
    Object[] positionalArgs = new Object[argsCount - 1];
    if (argsCount - 1 >= 0) System.arraycopy(arguments, 1, positionalArgs, 0, argsCount - 1);
    try {
      return interopLibrary.invokeMember(jsFun, "apply", arguments[0], new Array(positionalArgs));
    } catch (UnsupportedMessageException
        | UnsupportedTypeException
        | UnknownIdentifierException
        | ArityException e) {
      e.printStackTrace();
      throw new RuntimeException("oopsie");
    }
  }
}
