package org.enso.interpreter.node.expression.builtin.interop.syntax;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

@ReportPolymorphism
public abstract class HostValueToEnsoNode extends Node {
  public static HostValueToEnsoNode build() {
    return HostValueToEnsoNodeGen.create();
  }

  public abstract Object execute(Object o);

  @Specialization
  double doFloat(float f) {
    return f;
  }

  @Specialization
  long doInt(int i) {
    return i;
  }

  @Specialization
  long doShort(short i) {
    return i;
  }

  @Specialization
  long doByte(byte i) {
    return i;
  }

  @Specialization
  long doChar(char i) {
    return i;
  }

  @Fallback
  Object doOther(Object o) {
    return o;
  }
}
