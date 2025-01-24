package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import java.math.BigInteger;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoMultiType.AllTypesWith;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.Pair;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
public final class EnsoMultiValue extends EnsoObject {
  private final EnsoMultiType dispatch;
  private final EnsoMultiType extra;
  private final int firstDispatch;

  @CompilationFinal(dimensions = 1)
  private final Object[] values;

  private EnsoMultiValue(
      EnsoMultiType dispatch, EnsoMultiType extra, Object[] values, int firstDispatch) {
    this.firstDispatch = firstDispatch;
    this.dispatch = dispatch;
    this.extra = extra;
    this.values = values;
  }

  /** Creates new instance of EnsoMultiValue from provided information. */
  @GenerateUncached
  public abstract static class NewNode extends Node {
    private static final String INLINE_CACHE_LIMIT = "5";

    @NeverDefault
    public static NewNode create() {
      return EnsoMultiValueFactory.NewNodeGen.create();
    }

    @NeverDefault
    public static NewNode getUncached() {
      return EnsoMultiValueFactory.NewNodeGen.getUncached();
    }

    /**
     * Creates new multi value from provided information.
     *
     * @param types all the types this value can be {@link CastToNode cast to}
     * @param dispatchTypes the (subset of) types that the value is cast to currently - bigger than
     *     {@code 0} and at most {@code type.length}
     * @param firstDispatch location of first dispatch type in the values
     * @param values value of each of the provided {@code types}
     * @return non-{@code null} multi value instance
     */
    @NeverDefault
    public EnsoMultiValue newValue(
        @NeverDefault Type[] types,
        @NeverDefault int dispatchTypes,
        @NeverDefault int firstDispatch,
        @NeverDefault Object... values) {
      assert firstDispatch >= 0;
      assert dispatchTypes > 0;
      assert dispatchTypes <= types.length;
      assert types.length == values.length;
      assert firstDispatch + dispatchTypes <= types.length;
      assert !Stream.of(values).anyMatch(v -> v instanceof EnsoMultiValue)
          : "Avoid double wrapping " + Arrays.toString(values);
      var dt = executeTypes(types, 0, dispatchTypes);
      var et = executeTypes(types, dispatchTypes, types.length);
      assert !dt.hasIntersectionWith(et)
          : "Dispatch (" + dt + " and extra " + et + " should be disjoin!";
      return new EnsoMultiValue(dt, et, values, firstDispatch);
    }

    abstract EnsoMultiType executeTypes(Type[] types, int from, int to);

    @Specialization(
        guards = {"compareTypes(cachedTypes, types, from, to)"},
        limit = INLINE_CACHE_LIMIT)
    final EnsoMultiType cachedMultiType(
        Type[] types,
        int from,
        int to,
        @Cached(value = "clone(types, from, to)", dimensions = 1) Type[] cachedTypes,
        @Cached("createMultiType(types, from, to)") EnsoMultiType result) {
      return result;
    }

    @Specialization(replaces = "cachedMultiType")
    final EnsoMultiType createMultiType(Type[] types, int from, int to) {
      return EnsoMultiType.findOrCreateSlow(types, from, to);
    }

    @TruffleBoundary
    static final Type[] clone(Type[] types, int from, int to) {
      return Arrays.copyOfRange(types, from, to);
    }

    @ExplodeLoop
    static final boolean compareTypes(Type[] cached, Type[] arr, int from, int to) {
      CompilerAsserts.partialEvaluationConstant(cached);
      if (cached.length != to - from) {
        return false;
      }
      CompilerAsserts.partialEvaluationConstant(cached.length);
      for (var i = 0; i < cached.length; i++) {
        CompilerAsserts.partialEvaluationConstant(cached[i]);
        if (cached[i] != arr[from++]) {
          return false;
        }
      }
      return true;
    }
  }

  /**
   * The "dispatch identity" of the multi value. If two multivalues have the same identity, they are
   * going to resolve methods the same way.
   *
   * @return an opaque object that can be used for caching dispatch logic
   */
  public final Object getDispatchId() {
    // intentionally typed as Object to avoid exposing EnsoMultiType
    return dispatch;
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
  final Type getType() {
    return dispatch.firstType();
  }

  @ExportMessage
  final Type[] allTypes(
      boolean includeExtraTypes, @Cached EnsoMultiType.AllTypesWith allTypesWith) {
    if (!includeExtraTypes) {
      return allTypesWith.executeAllTypes(dispatch, null, 0);
    } else {
      return allTypesWith.executeAllTypes(dispatch, extra, 0);
    }
  }

  @ExportMessage
  @TruffleBoundary
  @Override
  public final String toDisplayString(boolean ignore) {
    return toString();
  }

  private enum InteropType {
    NULL,
    BOOLEAN,
    DATE_TIME_ZONE,
    DURATION,
    STRING,
    NUMBER,
    POINTER,
    META_OBJECT,
    ITERATOR;

    private record Value(InteropType type, Object value) {}

    static Value find(Object[] values, int firstDispatch, int max, InteropLibrary iop) {
      for (var i = 0; i < max; i++) {
        var v = values[firstDispatch + i];
        if (iop.isNull(v)) {
          return new Value(NULL, v);
        }
        if (iop.isBoolean(v)) {
          return new Value(BOOLEAN, v);
        }
        if (iop.isDate(v) || iop.isTime(v) || iop.isTimeZone(v)) {
          return new Value(DATE_TIME_ZONE, v);
        }
        if (iop.isDuration(v)) {
          return new Value(DURATION, v);
        }
        if (iop.isString(v)) {
          return new Value(STRING, v);
        }
        if (iop.isNumber(v)) {
          return new Value(NUMBER, v);
        }
        if (iop.isPointer(v)) {
          return new Value(POINTER, v);
        }
        if (iop.isMetaObject(v)) {
          return new Value(META_OBJECT, v);
        }
        if (iop.isIterator(v)) {
          return new Value(ITERATOR, v);
        }
      }
      return new Value(null, null);
    }
  }

  private InteropType.Value findInteropTypeValue(InteropLibrary iop) {
    return InteropType.find(values, firstDispatch, dispatch.typesLength(), iop);
  }

  @ExportMessage
  boolean isBoolean(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.BOOLEAN;
  }

  @ExportMessage
  boolean asBoolean(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.BOOLEAN) {
      return iop.asBoolean(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isString(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.STRING;
  }

  @ExportMessage
  String asString(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.STRING) {
      return iop.asString(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isNumber(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER;
  }

  @ExportMessage
  boolean fitsInByte(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInByte(both.value());
  }

  @ExportMessage
  boolean fitsInShort(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInShort(both.value());
  }

  @ExportMessage
  boolean fitsInInt(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInInt(both.value());
  }

  @ExportMessage
  boolean fitsInLong(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInLong(both.value());
  }

  @ExportMessage
  boolean fitsInFloat(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInFloat(both.value());
  }

  @ExportMessage
  boolean fitsInDouble(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInDouble(both.value());
  }

  @ExportMessage
  byte asByte(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asByte(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  short asShort(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asShort(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  int asInt(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asInt(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  long asLong(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asLong(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  float asFloat(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asFloat(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  double asDouble(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asDouble(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean fitsInBigInteger(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.NUMBER && iop.fitsInBigInteger(both.value());
  }

  @ExportMessage
  BigInteger asBigInteger(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.NUMBER) {
      return iop.asBigInteger(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isTime(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.DATE_TIME_ZONE && iop.isTime(both.value());
  }

  @ExportMessage
  LocalTime asTime(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.DATE_TIME_ZONE) {
      return iop.asTime(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isDate(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.DATE_TIME_ZONE && iop.isDate(both.value());
  }

  @ExportMessage
  LocalDate asDate(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.DATE_TIME_ZONE) {
      return iop.asDate(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isTimeZone(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.DATE_TIME_ZONE && iop.isTimeZone(both.value());
  }

  @ExportMessage
  ZoneId asTimeZone(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.DATE_TIME_ZONE) {
      return iop.asTimeZone(both.value());
    }
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isDuration(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var both = findInteropTypeValue(iop);
    return both.type() == InteropType.DURATION;
  }

  @ExportMessage
  Duration asDuration(@Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop)
      throws UnsupportedMessageException {
    var both = findInteropTypeValue(iop);
    if (both.type() == InteropType.DURATION) {
      return iop.asDuration(both.value());
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
    for (var i = 0; i < dispatch.typesLength(); i++) {
      try {
        var members = iop.getMembers(values[firstDispatch + i]);
        var len = iop.getArraySize(members);
        for (var j = 0L; j < len; j++) {
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
    for (var i = 0; i < dispatch.typesLength(); i++) {
      if (iop.isMemberInvocable(values[firstDispatch + i], name)) {
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
    for (var i = 0; i < dispatch.typesLength(); i++) {
      if (iop.isMemberInvocable(values[firstDispatch + i], name)) {
        return iop.invokeMember(values[firstDispatch + i], name, args);
      }
    }
    throw UnknownIdentifierException.create(name);
  }

  @TruffleBoundary
  @Override
  public String toString() {
    var both = EnsoMultiType.AllTypesWith.getUncached().executeAllTypes(dispatch, extra, 0);
    return Stream.of(both).map(t -> t.getName()).collect(Collectors.joining(" & "));
  }

  /** Casts {@link EnsoMultiValue} to requested type effectively. */
  public static final class CastToNode extends Node {
    private static final CastToNode UNCACHED =
        new CastToNode(
            EnsoMultiType.FindIndexNode.getUncached(),
            NewNode.getUncached(),
            AllTypesWith.getUncached());
    @Child private EnsoMultiType.FindIndexNode findNode;
    @Child private NewNode newNode;
    @Child private AllTypesWith allTypesWith;

    private CastToNode(EnsoMultiType.FindIndexNode f, NewNode n, AllTypesWith a) {
      this.findNode = f;
      this.newNode = n;
      this.allTypesWith = a;
    }

    @NeverDefault
    public static CastToNode create() {
      return new CastToNode(
          EnsoMultiType.FindIndexNode.create(), NewNode.create(), AllTypesWith.create());
    }

    @NeverDefault
    @TruffleBoundary
    public static CastToNode getUncached() {
      return UNCACHED;
    }

    /**
     * Casts value in a multi value into specific type.
     *
     * @param type the requested type
     * @param mv a multi value
     * @param reorderOnly allow (modified) {@link EnsoMultiValue} to be returned otherwise extract
     *     the value of {@code type} and return it directly
     * @param allTypes should we search all types or just up to {@code methodDispatchTypes}
     * @return instance of the {@code type} or {@code null} if no suitable value was found
     */
    public final Object findTypeOrNull(
        Type type, EnsoMultiValue mv, boolean reorderOnly, boolean allTypes) {
      var dispatch = mv.dispatch;
      var typeIndex = findNode.executeFindIndex(type, dispatch);
      var valueIndex = -1;
      if (typeIndex == -1) {
        if (allTypes) {
          var extraIndex = findNode.executeFindIndex(type, mv.extra);
          if (extraIndex != -1) {
            if (extraIndex < mv.firstDispatch) {
              valueIndex = extraIndex;
            } else {
              var rem = extraIndex - mv.firstDispatch;
              valueIndex = mv.firstDispatch + dispatch.typesLength() + rem;
              assert typeIndex < mv.values.length;
            }
            typeIndex = dispatch.typesLength() + extraIndex;
          }
        }
      } else {
        valueIndex = mv.firstDispatch + typeIndex;
      }
      if (typeIndex != -1) {
        if (reorderOnly) {
          var copyTypes = allTypesWith.executeAllTypes(dispatch, mv.extra, typeIndex);
          return newNode.newValue(copyTypes, 1, valueIndex, mv.values);
        } else {
          return mv.values[valueIndex];
        }
      } else {
        return null;
      }
    }
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
    for (var t : EnsoMultiType.AllTypesWith.getUncached().executeAllTypes(dispatch, null, 0)) {
      var fnAndType = node.execute(t, symbol);
      if (fnAndType != null) {
        if (dispatch.typesLength() == 1 || fnAndType.getRight() != ctx.getBuiltins().any()) {
          return Pair.create(fnAndType.getLeft(), t);
        }
        foundAnyMethod = fnAndType;
      }
    }
    return foundAnyMethod;
  }
}
