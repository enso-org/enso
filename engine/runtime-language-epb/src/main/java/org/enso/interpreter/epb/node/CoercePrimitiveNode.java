package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.strings.TruffleString;

@GenerateUncached
@ReportPolymorphism
public abstract class CoercePrimitiveNode extends Node {

  /**
   * Create a new node responsible for coercing primitive values to Enso primitives at the polyglot
   * boundary.
   *
   * @return a new primitive coercion node
   */
  public static CoercePrimitiveNode build() {
    return CoercePrimitiveNodeGen.create();
  }

  /**
   * Converts an object from a polyglot representation into an equivalent representation as an Enso
   * primitive.
   *
   * @param value the polyglot value to perform coercion on
   * @return {@code value} coerced to an Enso primitive where applicable
   */
  public abstract Object execute(Object value);

  @Specialization
  boolean doBoolean(boolean bool) {
    return bool;
  }

  @Specialization
  long doByte(byte n) {
    return n;
  }

  @Specialization
  long doShort(short n) {
    return n;
  }

  @Specialization
  long doInt(int n) {
    return n;
  }

  @Specialization
  long doLong(long n) {
    return n;
  }

  @Specialization
  double doFloat(float n) {
    return n;
  }

  @Specialization
  double doDouble(double n) {
    return n;
  }

  @Specialization
  long doChar(char character) {
    return character;
  }

  @Specialization
  String doString(String s) {
    return s;
  }

  @Specialization
  String doTruffleString(
      TruffleString truffleString, @Cached TruffleString.ToJavaStringNode toJavaStringNode) {
    return toJavaStringNode.execute(truffleString);
  }

  @Specialization
  Object doNonPrimitive(TruffleObject value) {
    return value;
  }
}
