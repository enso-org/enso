package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Meta",
    name = "type_of",
    description = "Returns the type of a value.",
    autoRegister = false)
@GenerateUncached
public abstract class TypeOfNode extends Node {

  public abstract Object execute(@AcceptsError Object value);

  public static TypeOfNode build() {
    return TypeOfNodeGen.create();
  }

  @Specialization
  Object doUnresolvedSymbol(UnresolvedSymbol value) {
    return EnsoContext.get(this).getBuiltins().function();
  }

  @Specialization
  Object doDouble(double value) {
    return EnsoContext.get(this).getBuiltins().number().getDecimal();
  }

  @Specialization
  Object doLong(long value) {
    return EnsoContext.get(this).getBuiltins().number().getInteger();
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger value) {
    return EnsoContext.get(this).getBuiltins().number().getInteger();
  }

  @Specialization
  Object doString(String value) {
    return EnsoContext.get(this).getBuiltins().text();
  }

  @Specialization
  Object doPanicException(PanicException value) {
    return EnsoContext.get(this).getBuiltins().panic();
  }

  @Specialization
  Object doPanicSentinel(PanicSentinel value) {
    return EnsoContext.get(this).getBuiltins().panic();
  }

  @Specialization
  Object doWarning(WithWarnings value) {
    return execute(value.getValue());
  }

  @Specialization(guards = {"!types.hasType(value)"})
  Object withoutType(
      Object value,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached WithoutType delegate) {
    return delegate.execute(value);
  }

  @Specialization(guards = {"types.hasType(value)", "!interop.isNumber(value)"})
  Object doType(
      Object value,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") TypesLibrary types) {
    return types.getType(value);
  }

  @Fallback
  @CompilerDirectives.TruffleBoundary
  Object doAny(Object value) {
    return DataflowError.withoutTrace(
        EnsoContext.get(this)
            .getBuiltins()
            .error()
            .makeCompileError("unknown type_of for " + value),
        this);
  }

  @GenerateUncached
  abstract static class WithoutType extends Node {
    abstract Object execute(Object value);

    @Specialization(guards = {"findInterop(value, interop).isArray()"})
    Type doPolyglotArray(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      return EnsoContext.get(this).getBuiltins().array();
    }

    @Specialization(guards = {"findInterop(value, interop).isString()"})
    Type doPolyglotString(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      return EnsoContext.get(this).getBuiltins().text();
    }

    @Specialization(guards = {"findInterop(value, interop).isNumber()"})
    Type doPolyglotNumber(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      if (interop.fitsInInt(value)) {
        return builtins.number().getInteger();
      } else if (interop.fitsInDouble(value)) {
        return builtins.number().getDecimal();
      } else {
        return EnsoContext.get(this).getBuiltins().number().getNumber();
      }
    }

    @Specialization(guards = {"findInterop(value, interop).isDateTime()"})
    Type doDateTime(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      return EnsoContext.get(this).getBuiltins().dateTime();
    }

    @Specialization(guards = {"findInterop(value, interop).isTimeZone()"})
    Type doTimeZone(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().timeZone();
    }

    @Specialization(guards = {"findInterop(value, interop).isDate()"})
    Type doDate(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().date();
    }

    @Specialization(guards = {"findInterop(value, interop).isTime()"})
    Type doTime(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().timeOfDay();
    }

    @Specialization(guards = "findInterop(value, interop).isDuration()")
    Type doDuration(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().duration();
    }

    @Specialization(guards = {"findInterop(value, interop).isNone()"})
    Object doMetaObject(Object value, @CachedLibrary(limit = "3") InteropLibrary interop) {
      try {
        return interop.getMetaObject(value);
      } catch (UnsupportedMessageException e) {
        CompilerDirectives.transferToInterpreter();
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        throw new PanicException(builtins.error().makeCompileError("invalid meta object"), this);
      }
    }

    static InteropType findInterop(Object value, InteropLibrary interop) {
      return InteropType.find(value, interop);
    }

    enum InteropType {
      NONE,
      STRING,
      NUMBER,
      ARRAY,
      DATE_TIME,
      TIME_ZONE,
      DATE,
      TIME,
      DURATION;

      static InteropType find(Object value, InteropLibrary interop) {
        if (interop.isString(value)) {
          return STRING;
        }
        if (interop.isNumber(value)) {
          return NUMBER;
        }
        if (interop.hasArrayElements(value)) {
          return ARRAY;
        }
        boolean time = interop.isTime(value);
        boolean date = interop.isDate(value);
        if (time) {
          return date ? DATE_TIME : TIME;
        }
        if (date) {
          return DATE;
        }
        if (interop.isTimeZone(value)) {
          return TIME_ZONE;
        }
        if (interop.isDuration(value)) {
          return DURATION;
        }
        return NONE;
      }

      boolean isString() {
        return this == STRING;
      }

      boolean isNumber() {
        return this == NUMBER;
      }

      boolean isArray() {
        return this == ARRAY;
      }

      boolean isDateTime() {
        return this == DATE_TIME;
      }

      boolean isTimeZone() {
        return this == TIME_ZONE;
      }

      boolean isTime() {
        return this == TIME;
      }

      boolean isDate() {
        return this == DATE;
      }

      boolean isDuration() {
        return this == DURATION;
      }

      boolean isNone() {
        return this == NONE;
      }
    }
  }
}
