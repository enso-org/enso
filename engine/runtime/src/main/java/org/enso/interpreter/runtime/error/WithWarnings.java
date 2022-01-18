package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

@ExportLibrary(MethodDispatchLibrary.class)
public class WithWarnings implements TruffleObject {
  private final ArrayRope<Object> warnings;
  private final Object value;

  public WithWarnings(Object value, Object... warnings) {
    this.warnings = new ArrayRope<>(warnings);
    this.value = value;
  }

  private WithWarnings(Object value, ArrayRope<Object> warnings) {
    this.warnings = warnings;
    this.value = value;
  }

  public Object getValue() {
    return value;
  }

  public WithWarnings inherit(WithWarnings that) {
    return new WithWarnings(value, warnings.append(that.warnings));
  }

  public WithWarnings append(Object... newWarnings) {
    return new WithWarnings(value, warnings.append(newWarnings));
  }

  public WithWarnings prepend(Object... newWarnings) {
    return new WithWarnings(value, warnings.prepend(newWarnings));
  }

  public Object[] getWarningsArray() {
    Object[] result = new Object[warnings.size()];
    warnings.writeArray(result);
    return result;
  }

  public ArrayRope<Object> getWarnings() {
    return warnings;
  }

  public static WithWarnings appendTo(Object target, ArrayRope<Object> warnings) {
    if (target instanceof WithWarnings) {
      return new WithWarnings(((WithWarnings) target).warnings.append(warnings));
    } else {
      return new WithWarnings(target, warnings);
    }
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }
}
