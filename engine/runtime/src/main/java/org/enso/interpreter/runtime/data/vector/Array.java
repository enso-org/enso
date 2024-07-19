package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.util.Arrays;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

/** A primitive boxed array type for use in the runtime. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Data.Array.Array")
final class Array implements EnsoObject {
  private final Object[] items;
  private Boolean withWarnings;
  private Warning[] cachedWarningsWrapped;
  private Warning[] cachedWarningsUnwrapped;

  /**
   * Creates a new array
   *
   * @param items the element values
   */
  private Array(Object... items) {
    this.items = items;
  }

  static Array wrap(Object... items) {
    assert noNulls(items);
    return new Array(items);
  }

  static Array allocate(long size) {
    var arr = new Object[Math.toIntExact(size)];
    return new Array(arr);
  }

  private static boolean noNulls(Object[] arr) {
    for (Object o : arr) {
      if (o == null) {
        return false;
      }
    }
    return true;
  }

  /**
   * @return the elements of this array as a java array.
   */
  final Object[] getItems() {
    return items;
  }

  /**
   * Marks the object as array-like for Polyglot APIs.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  /**
   * Handles reading an element by index through the polyglot API.
   *
   * @param index the index to read
   * @return the element value at the provided index
   * @throws InvalidArrayIndexException when the index is out of bounds.
   */
  @ExportMessage
  Object readArrayElement(
      long index,
      @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Cached BranchProfile errProfile,
      @Cached BranchProfile hasWarningsProfile,
      @Cached HashMapInsertNode mapInsertNode,
      @Cached AppendWarningNode appendWarningNode,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    if (index >= items.length || index < 0) {
      errProfile.enter();
      throw InvalidArrayIndexException.create(index);
    }

    var v = items[(int) index];
    if (this.hasWarnings(warnings)) {
      hasWarningsProfile.enter();
      Warning[] extracted = this.getWarnings(null, false, warnings, mapInsertNode, atNode, lengthNode);
      if (warnings.hasWarnings(v)) {
        v = warnings.removeWarnings(v);
      }
      return appendWarningNode.execute(null, v, extracted);
    }

    return v;
  }

  long length() {
    return items.length;
  }

  /**
   * Exposes the size of this collection through the polyglot API.
   *
   * @return the size of this array
   */
  @ExportMessage
  long getArraySize() {
    return items.length;
  }

  /**
   * Exposes an index validity check through the polyglot API.
   *
   * @param index the index to check
   * @return {@code true} if the index is valid, {@code false} otherwise.
   */
  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index < getArraySize() && index >= 0;
  }

  @ExportMessage
  String toDisplayString(boolean b) {
    return toString();
  }

  @ExportMessage
  Type getMetaObject(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().array();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return Arrays.toString(items);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  private boolean hasWarningElements(Object[] items, WarningsLibrary warnings) {
    for (Object item : items) {
      if (warnings.hasWarnings(item)) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean hasWarnings(@Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    if (withWarnings == null) {
      withWarnings = hasWarningElements(items, warnings);
    }
    return withWarnings;
  }

  @ExportMessage
  Warning[] getWarnings(
      Node location,
      boolean shouldWrap,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Shared("mapInsertNode") @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached ArrayLikeAtNode atNode,
      @Shared @Cached ArrayLikeLengthNode lengthNode)
      throws UnsupportedMessageException {
    Warning[] cache = shouldWrap ? cachedWarningsWrapped : cachedWarningsUnwrapped;
    if (cache == null) {
      var allWarnsMap = collectAllWarnings(warnings, location, mapInsertNode, shouldWrap);
      cache = Warning.fromMapToArray(allWarnsMap, lengthNode, atNode);
      if (shouldWrap) {
        cachedWarningsWrapped = cache;
      } else {
        cachedWarningsUnwrapped = cache;
      }
    }
    return cache;
  }

  private EnsoHashMap collectAllWarnings(
      WarningsLibrary warningsLib,
      Node location,
      HashMapInsertNode mapInsertNode,
      boolean shouldWrap)
      throws UnsupportedMessageException {
    var warnsSet = EnsoHashMap.empty();
    for (int itemIdx = 0; itemIdx < this.items.length; itemIdx++) {
      Object item = this.items[itemIdx];
      if (warningsLib.hasWarnings(item)) {
        Warning[] warnings = warningsLib.getWarnings(item, location, shouldWrap);
        Warning[] wrappedWarningsMaybe;

        if (shouldWrap) {
          wrappedWarningsMaybe = new Warning[warnings.length];
          for (int warnIdx = 0; warnIdx < warnings.length; warnIdx++) {
            wrappedWarningsMaybe[warnIdx] =
                Warning.wrapMapError(warningsLib, warnings[warnIdx], itemIdx);
          }
        } else {
          wrappedWarningsMaybe = warnings;
        }

        for (var warn : wrappedWarningsMaybe) {
          warnsSet = mapInsertNode.execute(null, warnsSet, warn, null);
        }
      }
    }
    return warnsSet;
  }

  @ExportMessage
  Array removeWarnings(@Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    Object[] items = new Object[this.items.length];
    for (int i = 0; i < this.items.length; i++) {
      if (warnings.hasWarnings(this.items[i])) {
        items[i] = warnings.removeWarnings(this.items[i]);
      } else {
        items[i] = this.items[i];
      }
    }
    return new Array(items);
  }

  @ExportMessage
  boolean isLimitReached(
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnsLib,
      @Shared("mapInsertNode") @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached ArrayLikeAtNode atNode,
      @Shared @Cached ArrayLikeLengthNode lengthNode) {
    try {
      int limit = EnsoContext.get(warnsLib).getWarningsLimit();
      var ourWarnings = getWarnings(null, false, warnsLib, mapInsertNode, atNode, lengthNode);
      return ourWarnings.length >= limit;
    } catch (UnsupportedMessageException e) {
      return false;
    }
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().array();
  }
}
