package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Meta",
    name = "type_of_builtin",
    description = "Returns the type of a value.")
public abstract class TypeOfNode extends Node {

  abstract Object execute(@AcceptsError Object value);

  static TypeOfNode build() {
    return TypeOfNodeGen.create();
  }

  @Specialization
  Object doDouble(double value) {
    Context ctx = Context.get(this);
    return ctx.getBuiltins().number().getDecimal();
  }

  @Specialization
  Object doLong(long value) {
    Context ctx = Context.get(this);
    return ctx.getBuiltins().number().getInteger();
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger value) {
    Context ctx = Context.get(this);
    return ctx.getBuiltins().number().getInteger();
  }

  @Specialization(guards = "isError(value)")
  Object doError(Object value) {
    return Context.get(this).getBuiltins().dataflowError();
  }

  boolean isError(Object value) {
    return TypesGen.isDataflowError(value);
  }

  @Specialization(guards = "interop.hasMetaObject(value)")
  Object doMetaObject(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return interop.getMetaObject(value);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      Builtins builtins = Context.get(this).getBuiltins();
      throw new PanicException(builtins.error().makeCompileError("invalid meta object"), this);
    }
  }

  @Specialization(guards = "types.hasType(value)")
  Object doType(Object value, @CachedLibrary(limit = "3") TypesLibrary types) {
    return types.getType(value);
  }

  @Fallback
  Object doAny(Object value) {
    return Context.get(this).getBuiltins().any();
  }
}
