package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.math.BigInteger;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.TreeSet;
import java.util.stream.Collectors;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.Pair;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
public final class EnsoMultiValue implements EnsoObject {

  @CompilationFinal(dimensions = 1)
  private final Type[] types;

  @CompilationFinal(dimensions = 1)
  private final Object[] values;

  private EnsoMultiValue(Type[] types, Object[] values) {
    this.types = types;
    assert types.length == values.length;
    this.values = values;
  }

  public static EnsoObject create(Type[] types, Object[] values) {
    return new EnsoMultiValue(types, values);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  @ExportMessage
  public final Type getType() {
    return types[0];
  }

  public final Type[] allTypes() {
    return types.clone();
  }

  @ExportMessage
  String toDisplayString(boolean ignore) {
    return toString();
  }

  @ExportMessage
  boolean isBoolean(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isBoolean(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean asBoolean(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.isBoolean(values[i])) {
        return iop.asBoolean(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isString(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isString(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  String asString(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (Object value : values) {
      if (iop.isString(value)) {
        return iop.asString(value);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isNumber(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isNumber(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean fitsInByte(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInByte(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean fitsInShort(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInShort(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean fitsInInt(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInShort(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean fitsInLong(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInLong(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean fitsInFloat(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInFloat(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean fitsInDouble(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInDouble(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  byte asByte(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInByte(values[i])) {
        return iop.asByte(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  short asShort(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInShort(values[i])) {
        return iop.asShort(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  int asInt(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInInt(values[i])) {
        return iop.asInt(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  long asLong(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInLong(values[i])) {
        return iop.asLong(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  float asFloat(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInFloat(values[i])) {
        return iop.asFloat(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  double asDouble(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInDouble(values[i])) {
        return iop.asDouble(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean fitsInBigInteger(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInBigInteger(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  BigInteger asBigInteger(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.fitsInBigInteger(values[i])) {
        return iop.asBigInteger(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isTime(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isTime(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  LocalTime asTime(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.isTime(values[i])) {
        return iop.asTime(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isDate(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isDate(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  LocalDate asDate(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.isDate(values[i])) {
        return iop.asDate(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isTimeZone(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isTimeZone(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  ZoneId asTimeZone(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.isTimeZone(values[i])) {
        return iop.asTimeZone(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isDuration(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isDuration(values[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  Duration asDuration(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    for (var i = 0; i < values.length; i++) {
      if (iop.isDuration(values[i])) {
        return iop.asDuration(values[i]);
      }
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  @ExportMessage
  @TruffleBoundary
  Object getMembers(
      boolean includeInternal, @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var names = new TreeSet<String>();
    for (var i = 0; i < values.length; i++) {
      try {
        var members = iop.getMembers(values[i]);
        var len = iop.getArraySize(members);
        for (var j = 0L; j < len; i++) {
          var name = iop.readArrayElement(members, j);
          names.add(iop.asString(name));
        }
      } catch (InvalidArrayIndexException | UnsupportedMessageException ex) {
      }
    }
    return ArrayLikeHelpers.wrapObjectsWithCheckAt(names.toArray());
  }

  @ExportMessage
  boolean isMemberInvocable(
      String name, @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    for (var i = 0; i < values.length; i++) {
      if (iop.isMemberInvocable(values[i], name)) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  Object invokeMember(
      String name,
      Object[] args,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException,
          ArityException,
          UnsupportedTypeException,
          UnknownIdentifierException {
    for (var i = 0; i < values.length; i++) {
      if (iop.isMemberInvocable(values[i], name)) {
        return iop.invokeMember(values[i], name, args);
      }
    }
    throw UnknownIdentifierException.create(name);
  }

  @TruffleBoundary
  @Override
  public String toString() {
    return Arrays.stream(types).map(t -> t.getName()).collect(Collectors.joining(" & "));
  }

  /**
   * Casts value in this multi value into specific t.
   *
   * @param type the requested t
   * @return instance of the {@code t} or {@code null} if no suitable value was found
   */
  public final Object castTo(Type type) {
    for (var i = 0; i < types.length; i++) {
      if (types[i] == type) {
        return values[i];
      }
    }
    return null;
  }

  /**
   * Tries to resolve the symbol in one of multi value types.
   *
   * @param node resolution node to use
   * @param symbol symbol to resolve
   * @return {@code null} when no resolution was found or pair of function and type solved
   */
  public final Pair<Function, Type> resolveSymbol(
      MethodResolverNode node, UnresolvedSymbol symbol) {
    var ctx = EnsoContext.get(node);
    Pair<Function, Type> foundAnyMethod = null;
    for (Type t : types) {
      var fnAndType = node.execute(t, symbol);
      if (fnAndType != null) {
        if (fnAndType.getRight() != ctx.getBuiltins().any()) {
          return Pair.create(fnAndType.getLeft(), t);
        }
        foundAnyMethod = fnAndType;
      }
    }
    return foundAnyMethod;
  }
}
