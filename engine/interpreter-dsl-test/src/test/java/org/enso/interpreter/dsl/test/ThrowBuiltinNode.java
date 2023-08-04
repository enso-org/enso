package org.enso.interpreter.dsl.test;

import com.oracle.truffle.api.nodes.Node;
import java.util.function.Supplier;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "ThrowBuiltin", name = "throw")
public class ThrowBuiltinNode extends Node {
  public Object execute(long exceptionIdx) throws Throwable {
    Supplier<Throwable> exceptionSupplier =
        ThrowableCatchTest.exceptionSuppliers.get((int) exceptionIdx);
    throw exceptionSupplier.get();
  }
}
