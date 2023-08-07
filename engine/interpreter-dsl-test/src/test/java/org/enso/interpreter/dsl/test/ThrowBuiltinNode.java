package org.enso.interpreter.dsl.test;

import com.oracle.truffle.api.nodes.Node;
import java.util.function.Supplier;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "ThrowBuiltinNode", name = "throw")
public class ThrowBuiltinNode extends Node {
  public Object execute(Text type, long exceptionIdx) {
    switch (type.toString()) {
      case "exception" -> {
          Supplier<RuntimeException> exceptionSupplier =
              ThrowableCatchTest.exceptionSuppliers.get((int) exceptionIdx);
        throw exceptionSupplier.get();
      }
      case "error" -> {
        Supplier<Error> errorSupplier =
            ThrowableCatchTest.errorSuppliers.get((int) exceptionIdx);
        throw errorSupplier.get();
      }
      default -> throw new AssertionError("Unknown type: " + type);
    }
  }
}
