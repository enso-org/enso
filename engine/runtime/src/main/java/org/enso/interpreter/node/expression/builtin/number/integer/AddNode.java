package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.InvokeMethodNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(type = "Integer", name = "+", description = "Addition of numbers.")
public abstract class AddNode extends IntegerNode {

  public abstract Object execute(Object self, Object that);

  public static AddNode build() {
    return AddNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long self, long that) {
    return Math.addExact(self, that);
  }

  @Specialization(replaces = "doLong")
  Object doOverflow(long self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(self, that));
  }

  @Specialization
  Object doDouble(long self, double that) {
    return self + that;
  }

  @TruffleBoundary
  @Specialization
  Object doBigIntegers(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(self.asBigInteger().add(that.asBigInteger()));
  }

  @Specialization
  Object doLongBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(that.getValue(), self));
  }

  @Specialization
  Object doBigIntegerLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.add(self.getValue(), that));
  }

  @Specialization
  @TruffleBoundary
  double doBigIntDouble(EnsoBigInteger self, double that) {
    return self.getValue().doubleValue() + that;
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      Object self,
      TruffleObject that,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @Cached AddNode delegate) {
    return super.doInterop(self, that, iop, delegate);
  }

  @Specialization
  Object doOther(Object self, Object that) {
    var ctx = EnsoContext.get(this);
    var typeOfNode = TypeOfNode.getUncached();
    var rawType = typeOfNode.execute(that);
    ApplicationNode convertNode = null;
    Function conversionFn = null;
    if (rawType instanceof Type type) {
      var convert = UnresolvedConversion.build(type.getDefinitionScope());
      conversionFn = convert.resolveFor(ctx, type, ctx.getBuiltins().number().getInteger());
      if (conversionFn != null) {
        var convNode = LiteralNode.build(conversionFn);
        var intoNode = LiteralNode.build(type);
        var valueNode = LiteralNode.build((Long) self);
        var args =
            new CallArgument[] {
              new CallArgument(null, intoNode), new CallArgument(null, valueNode)
            };
        convertNode = ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);
      }
    }

    if (convertNode != null && rawType instanceof Type type) {
      var state = State.create(ctx);
      var frame =
          Truffle.getRuntime()
              .createVirtualFrame(
                  Function.ArgumentsHelper.buildArguments(conversionFn, state),
                  new FrameDescriptor());
      var convertedValue = convertNode.executeGeneric(frame);

      var symbol = UnresolvedSymbol.build("+", type.getDefinitionScope());
      var schema =
          new CallArgumentInfo[] {new CallArgumentInfo("self"), new CallArgumentInfo("that")};
      var invokeNode =
          InvokeMethodNode.build(
              schema, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE, 0, false);
      var result =
          invokeNode.execute(
              frame, state, symbol, convertedValue, new Object[] {convertedValue, that});
      return result;
    }

    throw throwTypeErrorIfNotInt(self, that);
  }
}
