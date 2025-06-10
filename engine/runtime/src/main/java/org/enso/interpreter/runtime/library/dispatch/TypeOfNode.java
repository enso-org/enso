package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.meta.AtomWithAHoleNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.warning.WithWarnings;

/**
 * Provides API (in form of {@code public} methods) for querying type information about a value.
 * Contains non-{@code public} methods with implementation to handle such queries.
 */
@GenerateUncached
public abstract class TypeOfNode extends Node {
  TypeOfNode() {}

  /**
   * Check whether given value has an Enso {@link Type}.
   *
   * @param value the value to check
   * @return {@code true} if there is (at least one) type associated with the value
   */
  public final boolean hasType(Object value) {
    return findTypeOrError(value) instanceof Type;
  }

  /**
   * Finds (a primary) type associated with a given {@code value}. If no such type exists, an error
   * value is returned that can normally flow thru the Enso interpreter without any conversions.
   *
   * @param value the value to check
   * @return either {@link Type} of the value, or meta object of the value, or {@link DataflowError}
   *     if there is no such type
   */
  public final TruffleObject findTypeOrError(Object value) {
    try {
      var types = executeTypes(value, false);
      return types[0];
    } catch (NonTypeResult plain) {
      return plain.result;
    }
  }

  /**
   * Finds (a primary) type associated with a given {@code value} or returns {@code null}. Useful
   * for internal checks in the Enso interpreter.
   *
   * @param value the value to check
   * @return either Type of the value or {@code null} if there is no such type
   */
  public final Type findTypeOrNull(Object value) {
    return findTypeOrError(value) instanceof Type type ? type : null;
  }

  /**
   * Finds all types associated with a given {@code value} or returns {@code null}. It is guaranteed
   * that the returned array is going to have at least one element, if it is non-{@code null}.
   *
   * @param value the value to check
   * @param includeExtraTypes specify {@code false} to return only <em>types value has already been
   *     case to</em>, specify {@code true} to return all <em>types value can be cast to</em>
   * @return either types associated with the value or {@code null} if there is no such type
   */
  public final Type[] findAllTypesOrNull(Object value, boolean includeExtraTypes) {
    try {
      var types = executeTypes(value, includeExtraTypes);
      assert types != null && types.length > 0;
      return types;
    } catch (NonTypeResult ex) {
      return null;
    }
  }

  /**
   * Internal implementation call to delegate to various {@link Specialization} methods.
   *
   * @param value the value to find type for
   * @param includeExtraTypes {@code false} to return only <em>types value has already been case
   *     to</em>, {@code true} to return all <em>types value can be cast to</em>
   * @return array of types with at least one element, but possibly more
   * @throws NonTypeResult when there is a <em>interop value</em> result, but it is not a {@link
   *     Type}
   */
  abstract Type[] executeTypes(Object value, boolean includeExtraTypes) throws NonTypeResult;

  /**
   * Creates new optimizing instance of this node.
   *
   * @return new instance of this node ready to be <em>specialized</em>
   */
  @NeverDefault
  public static TypeOfNode create() {
    return TypeOfNodeGen.create();
  }

  /**
   * Returns default, non-optimizing implementation of this node.
   *
   * @return shared singleton instance of this node
   */
  @NeverDefault
  public static TypeOfNode getUncached() {
    return TypeOfNodeGen.getUncached();
  }

  private static Type[] fromType(Type single) {
    return new Type[] {single};
  }

  @Specialization
  Type[] doUnresolvedSymbol(UnresolvedSymbol value, boolean allTypes) {
    return fromType(EnsoContext.get(this).getBuiltins().function());
  }

  @Specialization
  Type[] doDouble(double value, boolean allTypes) {
    return fromType(EnsoContext.get(this).getBuiltins().number().getFloat());
  }

  @Specialization
  Type[] doLong(long value, boolean allTypes) {
    return fromType(EnsoContext.get(this).getBuiltins().number().getInteger());
  }

  @Specialization
  Type[] doBigInteger(EnsoBigInteger value, boolean allTypes) {
    return fromType(EnsoContext.get(this).getBuiltins().number().getInteger());
  }

  @Specialization
  Type[] doPanicException(PanicException value, boolean allTypes) {
    return fromType(EnsoContext.get(this).getBuiltins().panic());
  }

  @Specialization
  Type[] doPanicSentinel(PanicSentinel value, boolean allTypes) {
    return fromType(EnsoContext.get(this).getBuiltins().panic());
  }

  @Specialization
  Type[] doWarning(WithWarnings value, boolean includeExtraTypes, @Cached TypeOfNode withoutWarning)
      throws NonTypeResult {
    return withoutWarning.executeTypes(value.getValue(), includeExtraTypes);
  }

  static boolean isWithType(Object value, TypesLibrary types, InteropLibrary iop) {
    if (value instanceof EnsoMultiValue) {
      return true;
    }
    if (iop.isNumber(value)) {
      return false;
    }
    return types.hasType(value);
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
  Type[] withoutType(
      Object value,
      boolean allTypes,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types,
      @Cached WithoutType delegate)
      throws NonTypeResult {
    var kind = WithoutType.Interop.resolve(value, interop);
    var typeOrPlain = delegate.execute(kind, value);
    if (typeOrPlain instanceof Type type) {
      return fromType(type);
    } else if (typeOrPlain instanceof TruffleObject metaObject) {
      throw new NonTypeResult(metaObject);
    } else {
      CompilerDirectives.transferToInterpreter();
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(
          this, "MetaObject should be a TruffleObject: " + typeOrPlain, null);
    }
  }

  @Specialization(guards = {"isWithType(value, types, interop)"})
  Type[] doType(
      Object value,
      boolean includeExtraTypes,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "3") TypesLibrary types) {
    return types.allTypes(value, includeExtraTypes);
  }

  @Fallback
  @CompilerDirectives.TruffleBoundary
  Type[] doAny(Object value, boolean allTypes) throws NonTypeResult {
    var err =
        DataflowError.withDefaultTrace(
            EnsoContext.get(this)
                .getBuiltins()
                .error()
                .makeCompileError("unknown type_of for " + value),
            this);
    throw new NonTypeResult(err);
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

  static final class NonTypeResult extends Exception {
    final TruffleObject result;

    NonTypeResult(TruffleObject result) {
      this.result = result;
    }

    @Override
    public Throwable fillInStackTrace() {
      return this;
    }
  }
}
