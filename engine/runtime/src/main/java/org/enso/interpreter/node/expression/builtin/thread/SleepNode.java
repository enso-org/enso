package org.enso.interpreter.node.expression.builtin.thread;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.control.ThreadInterruptedException;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Thread",
    name = "sleep",
    description = "Sleep in the current thread.",
    autoRegister = false,
    inlineable = true)
public abstract class SleepNode extends Node {

  public abstract Object execute(Object timeInMilliseconds);

  public static SleepNode build() {
    return SleepNodeGen.create();
  }

  @Specialization
  Object doLong(long timeInMilliseconds) {
    try {
      Thread.sleep(timeInMilliseconds);
      return EnsoContext.get(this).getBuiltins().nothing();
    } catch (InterruptedException e) {
      throw new ThreadInterruptedException();
    }
  }

  @Specialization
  Object doDouble(double timeInMilliseconds) {
    try {
      Thread.sleep(Double.valueOf(timeInMilliseconds).longValue());
      return EnsoContext.get(this).getBuiltins().nothing();
    } catch (InterruptedException e) {
      throw new ThreadInterruptedException();
    }
  }

  @Fallback
  Object doOther(Object timeInMilliseconds) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    throw new PanicException(
        builtins.error().makeTypeError(intType, timeInMilliseconds, "timeInMilliseconds"), this);
  }
}
