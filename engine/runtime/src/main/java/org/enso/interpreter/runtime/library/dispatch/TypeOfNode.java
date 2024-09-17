package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.meta.AtomWithAHoleNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.warning.WithWarnings;

@GenerateUncached
public abstract class TypeOfNode extends Node {
  TypeOfNode() {}

  public abstract Object execute(Object value);

  @NeverDefault
  public static TypeOfNode create() {
    return TypeOfNodeGen.create();
  }

  @NeverDefault
  public static TypeOfNode getUncached() {
    return TypeOfNodeGen.getUncached();
  }

  @Specialization
  Object doUnresolvedSymbol(UnresolvedSymbol value) {
    return EnsoContext.get(this).getBuiltins().function();
  }

  @Specialization
  Object doDouble(double value) {
    return EnsoContext.get(this).getBuiltins().number().getFloat();
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
  Object doPanicException(PanicException value) {
    return EnsoContext.get(this).getBuiltins().panic();
  }

  @Specialization
  Object doPanicSentinel(PanicSentinel value) {
    return EnsoContext.get(this).getBuiltins().panic();
  }

  @Specialization
  Object doWarning(WithWarnings value, @Cached TypeOfNode withoutWarning) {
    return withoutWarning.execute(value.getValue());
  }

  static boolean isWithoutType(Object value, TypesLibrary types) {
    if (value instanceof EnsoObject) {
      return false;
    }
    if (types.hasType(value)) {
      return false;
    }
    return true;
  }

  @Specialization(guards = {"isWithoutType(value, types)"})
  Object withoutType(
      Object value,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached WithoutType delegate) {
    var type = WithoutType.Interop.resolve(value, interop);
    return delegate.execute(type, value);
  }

  @Specialization(guards = {"types.hasType(value)", "!interop.isNumber(value)"})
  Object doType(
      Object value,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types) {
    return types.getType(value);
  }

  @Fallback
  @CompilerDirectives.TruffleBoundary
  Object doAny(Object value) {
    return DataflowError.withDefaultTrace(
        EnsoContext.get(this)
            .getBuiltins()
            .error()
            .makeCompileError("unknown type_of for " + value),
        this);
  }

  @GenerateUncached
  abstract static class WithoutType extends Node {
    abstract Object execute(Interop op, Object value);

    @Specialization(guards = {"type.isArray()"})
    Type doPolyglotArray(Interop type, Object value) {
      return EnsoContext.get(this).getBuiltins().array();
    }

    @Specialization(guards = {"type.isMap()"})
    Type doPolygotMap(Interop type, Object value) {
      return EnsoContext.get(this).getBuiltins().dictionary();
    }

    @Specialization(guards = {"type.isString()"})
    Type doPolyglotString(Interop type, Object value) {
      return EnsoContext.get(this).getBuiltins().text();
    }

    @Specialization(guards = {"type.isNumber()"})
    Type doPolyglotNumber(
        Interop type,
        Object value,
        @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      if (interop.fitsInLong(value)) {
        return builtins.number().getInteger();
      } else if (interop.fitsInBigInteger(value)) {
        return builtins.number().getInteger();
      } else if (interop.fitsInDouble(value)) {
        return builtins.number().getFloat();
      } else {
        return EnsoContext.get(this).getBuiltins().number().getNumber();
      }
    }

    @Specialization(guards = {"type.isDateTime()"})
    Type doDateTime(Interop type, Object value) {
      return EnsoContext.get(this).getBuiltins().dateTime();
    }

    @Specialization(guards = {"type.isTimeZone()"})
    Type doTimeZone(Interop type, Object value) {
      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().timeZone();
    }

    @Specialization(guards = {"type.isDate()"})
    Type doDate(Interop type, Object value) {

      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().date();
    }

    @Specialization(guards = {"type.isTime()"})
    Type doTime(Interop type, Object value) {

      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().timeOfDay();
    }

    @Specialization(guards = "type.isDuration()")
    Type doDuration(Interop type, Object value) {
      EnsoContext ctx = EnsoContext.get(this);
      return ctx.getBuiltins().duration();
    }

    @Specialization(guards = {"type.isMetaObject()"})
    Object doMetaObject(
        Interop type,
        Object value,
        @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
      try {
        return interop.getMetaObject(value);
      } catch (UnsupportedMessageException e) {
        CompilerDirectives.transferToInterpreter();
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        throw new PanicException(builtins.error().makeCompileError("invalid meta object"), this);
      }
    }

    @Fallback
    @CompilerDirectives.TruffleBoundary
    Object doAny(Interop any, Object value) {
      return DataflowError.withDefaultTrace(
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeCompileError("unknown type_of for " + value),
          this);
    }

    enum Interop {
      NONE,
      STRING,
      NUMBER,
      ARRAY,
      MAP,
      DATE_TIME,
      TIME_ZONE,
      DATE,
      TIME,
      DURATION,
      META_OBJECT;

      static Interop resolve(Object value, InteropLibrary interop) {
        assert !(value instanceof EnsoObject) || AtomWithAHoleNode.isHole(value)
            : "Don't use interop for EnsoObject: " + value.getClass().getName();
        if (interop.isString(value)) {
          return STRING;
        }
        if (interop.isNumber(value)) {
          return NUMBER;
        }
        if (interop.hasArrayElements(value)) {
          return ARRAY;
        }
        if (interop.hasHashEntries(value)) {
          return MAP;
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
        if (interop.hasMetaObject(value)) {
          return META_OBJECT;
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

      boolean isMap() {
        return this == MAP;
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

      boolean isMetaObject() {
        return this == META_OBJECT;
      }

      boolean isNone() {
        return this == NONE;
      }
    }
  }
}
