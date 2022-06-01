package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

import java.math.BigInteger;

@BuiltinMethod(
    type = "Any",
    name = "hash_code",
    description = "Gets the Hashcode value of an Enso object.")
public abstract class AnyHashCodeNode extends Node {
  private static final BigInteger bigInteger_MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);
  private static final BigInteger bigInteger_MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);

  public static AnyHashCodeNode build() {
    return AnyHashCodeNodeGen.create();
  }

  public abstract Object execute(Object _this);

  private static Long combineTypeAndHashCodes(int typeCode, int hashCode) {
    return Long.valueOf(typeCode << 7 + typeCode + hashCode);
  }

  @Specialization
  Object doBoolean(Boolean _this) {
    // _this.getClass().toString().hashCode() == 1335156652
    return combineTypeAndHashCodes(1335156652, _this.hashCode());
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this) {
    // _this.getClass().toString().hashCode() == 310304960
    BigInteger bigInteger = _this.getValue();
    if (bigInteger.compareTo(bigInteger_MAX_LONG) < 0 && bigInteger.compareTo(bigInteger_MIN_LONG) > 0) {
      return this.doLong(bigInteger.longValue());
    }
    return combineTypeAndHashCodes(310304960, bigInteger.hashCode());
  }

  @Specialization
  Object doLong(Long _this) {
    // _this.getClass().toString().hashCode() == -1228562056
    return combineTypeAndHashCodes(-1228562056, _this.hashCode());
  }

  @Specialization
  Object doDouble(Double _this) {
    // _this.getClass().toString().hashCode() == 239044557
    if (_this % 1 == 0 && _this <= Long.MAX_VALUE && _this >= Long.MIN_VALUE) {
      return this.doLong(_this.longValue());
    }
    return combineTypeAndHashCodes(239044557, Double.hashCode(_this));
  }

  @Specialization
  Object doAtom(Atom _this, @CachedLibrary(limit = "5") InteropLibrary atoms) {
    var fields = _this.getFields();
    int hashCode = 0;
    for (Object val : fields) {
      hashCode = hashCode << 5 + hashCode + ((Long)this.execute(hashCode)).intValue();
    }

    return combineTypeAndHashCodes(_this.getConstructor().getName().hashCode(), hashCode);
  }

  @Fallback
  Object useObject(Object _this) {
    return combineTypeAndHashCodes(_this.getClass().getName().hashCode(), _this.hashCode());
  }
}
