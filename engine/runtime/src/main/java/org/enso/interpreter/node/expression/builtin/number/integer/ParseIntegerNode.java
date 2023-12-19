package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.nodes.Node.Child;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Integer",
    name = "parse",
    description = """
Parse integer number""",
    autoRegister = false)
public final class ParseIntegerNode extends IntegerNode {
  @Child ToJavaStringNode toJavaString = ToJavaStringNode.build();
  private final BranchProfile noEx1 = BranchProfile.create();
  private final BranchProfile noEx2 = BranchProfile.create();

  Object execute(Text value, long radix) {
    var r = Math.toIntExact(radix);
    var t = toJavaString.execute(value);
    try {
      return Long.parseLong(t, r);
    } catch (NumberFormatException ex) {
      noEx1.enter();
      try {
        return new EnsoBigInteger(new BigInteger(t, r));
      } catch (NumberFormatException ex2) {
        ex = ex2;
      }
      noEx2.enter();
      var errors = EnsoContext.get(this).getBuiltins().error();
      var err = errors.makeNumberParseError(ex.getMessage());
      return DataflowError.withoutTrace(err, this);
    }
  }
}
