package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "insert_builtin",
    description = "Inserts a set of values into the Vector at the specified index.",
    autoRegister = false)
public abstract class InsertBuiltinVectorNode extends Node {
  static InsertBuiltinVectorNode build() {
    return InsertBuiltinVectorNodeGen.create();
  }

  abstract Vector execute(Vector self, long index, Object values);

  @Specialization
  Vector fromVector(Vector self, long index, Vector values, @Cached CoerceArrayNode coerce) {
    return insertBuiltin(coerce.execute(self), index, coerce.execute(values));
  }

  @Specialization(guards = "interop.hasArrayElements(values)")
  Vector fromArrayLikeObject(
      Vector self,
      long index,
      Object values,
      @Cached CoerceArrayNode coerce,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(coerce.execute(self), index, coerce.execute(values));
  }

  @Fallback
  Vector fromUnknown(Vector self, long index, Object values) {
    throw unsupportedException(values);
  }

  private PanicException unsupportedException(Object values) {
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError("polyglot array", values, "array");
    throw new PanicException(err, this);
  }

  private Vector insertBuiltin(Object[] current, long index, Object[] values) {
    Object[] result = new Object[current.length + values.length];
    System.arraycopy(current, 0, result, 0, (int) index);
    System.arraycopy(values, 0, result, (int) index, values.length);
    System.arraycopy(current, (int) index, result, (int) index + values.length, current.length - (int) index);
    return Vector.fromArray(new Array(result));
  }
}
