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
import java.util.Comparator;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertAllNode;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.data.hash.HashMapSizeNode;
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

  /** If true, some elements contain warning, and thus, this Array contains warning. */
  private Boolean withWarnings;

  private EnsoHashMap cachedWarningsWrapped;

  private EnsoHashMap cachedWarningsUnwrapped;

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
      @Cached BranchProfile shouldWrapProfile,
      @Cached HashMapSizeNode mapSizeNode,
      @Cached HashMapInsertAllNode mapInsertAllNode)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    if (index >= items.length || index < 0) {
      errProfile.enter();
      throw InvalidArrayIndexException.create(index);
    }

    var v = items[(int) index];
    if (this.hasWarnings(warnings)) {
      hasWarningsProfile.enter();
      var extractedWarnsMap =
          this.getWarnings(
              false, warnings, mapInsertNode, shouldWrapProfile, mapSizeNode, mapInsertAllNode);
      if (warnings.hasWarnings(v)) {
        v = warnings.removeWarnings(v);
      }
      return appendWarningNode.executeAppend(null, v, extractedWarnsMap);
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
  EnsoHashMap getWarnings(
      boolean shouldWrap,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Shared("mapInsertNode") @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached BranchProfile shouldWrapProfile,
      @Shared @Cached HashMapSizeNode mapSizeNode,
      @Shared @Cached HashMapInsertAllNode mapInsertAllNode)
      throws UnsupportedMessageException {
    var cache = shouldWrap ? cachedWarningsWrapped : cachedWarningsUnwrapped;
    if (cache == null) {
      var warnLimit = EnsoContext.get(warnings).getWarningsLimit();
      var allWarnsMap =
          collectAllWarnings(
              warnings,
              mapInsertNode,
              shouldWrap,
              mapInsertAllNode,
              warnLimit,
              shouldWrapProfile,
              mapSizeNode);
      if (shouldWrap) {
        cachedWarningsWrapped = allWarnsMap;
        cache = cachedWarningsWrapped;
      } else {
        cachedWarningsUnwrapped = allWarnsMap;
        cache = cachedWarningsUnwrapped;
      }
    }
    assert cache != null;
    return cache;
  }

  private EnsoHashMap collectAllWarnings(
      WarningsLibrary warningsLib,
      HashMapInsertNode mapInsertNode,
      boolean shouldWrap,
      HashMapInsertAllNode mapInsertAllNode,
      int warnLimit,
      BranchProfile shouldWrapProfile,
      HashMapSizeNode mapSizeNode)
      throws UnsupportedMessageException {
    var warnsSet = EnsoHashMap.empty();
    for (int itemIdx = 0; itemIdx < this.items.length; itemIdx++) {
      Object item = this.items[itemIdx];
      var warnsCnt = (int) mapSizeNode.execute(warnsSet);
      if (warnsCnt == warnLimit) {
        break;
      }
      if (warningsLib.hasWarnings(item)) {
        var itemWarnsMap = warningsLib.getWarnings(item, shouldWrap);
        assert mapSizeNode.execute(itemWarnsMap) <= warnLimit;

        if (!shouldWrap) {
          warnsSet =
              mapInsertAllNode.executeInsertAll(null, warnsSet, itemWarnsMap, warnLimit - warnsCnt);
        } else {
          shouldWrapProfile.enter();
          CompilerDirectives.transferToInterpreter();
          // warnings need to be sorted such that at the first index, there is the oldest warning.
          // This is because we are creating new warnings by wrapping the previous one, and we need
          // to
          // do that in the same creation order.
          var warnings = Warning.fromMapToArray(itemWarnsMap);
          Arrays.sort(warnings, Comparator.comparing(Warning::getSequenceId));
          for (int i = 0; i < Math.min(warnings.length, warnLimit); i++) {
            var warn = warnings[i];
            var wrappedWarn = Warning.wrapMapError(warningsLib, warn, itemIdx);
            warnsSet =
                mapInsertNode.execute(null, warnsSet, wrappedWarn.getSequenceId(), wrappedWarn);
          }
        }
      }
    }
    assert mapSizeNode.execute(warnsSet) <= warnLimit;
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
      @Shared @Cached HashMapInsertAllNode mapInsertAllNode,
      @Shared @Cached HashMapSizeNode mapSizeNode,
      @Shared @Cached BranchProfile shouldWrapProfile) {
    try {
      int limit = EnsoContext.get(warnsLib).getWarningsLimit();
      var ourWarnings =
          getWarnings(
              false, warnsLib, mapInsertNode, shouldWrapProfile, mapSizeNode, mapInsertAllNode);
      return (int) mapSizeNode.execute(ourWarnings) >= limit;
    } catch (UnsupportedMessageException e) {
      return false;
    }
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().array();
  }
}
