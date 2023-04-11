package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.util.Arrays;
import org.enso.interpreter.runtime.error.WithWarnings;

/** A primitive boxed array type for use in the runtime. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Data.Array.Array")
public final class Array implements TruffleObject {
  private final Object[] items;
  private Boolean withWarnings;

  /**
   * Creates a new array
   *
   * @param items the element values
   */
  @Builtin.Method(
      expandVarargs = 4,
      description = "Creates an array with given elements.",
      autoRegister = false)
  public Array(Object... items) {
    assert noNulls(items);
    this.items = items;
  }

  /**
   * Creates an uninitialized array of the given size.
   *
   * @param size the size of the created array.
   */
  @Builtin.Method(
      description = "Creates an uninitialized array of a given size.",
      autoRegister = false,
      name = "new")
  public static Array allocate(long size) {
    var arr = new Object[(int) size];
    var ctx = EnsoContext.get(null);
    var nothing = ctx.getBuiltins().nothing();
    for (int i = 0; i < arr.length; i++) {
      arr[i] = nothing;
    }
    return new Array(arr);
  }

  private static final boolean noNulls(Object[] arr) {
    for (int i = 0; i < arr.length; i++) {
      if (arr[i] == null) {
        return false;
      }
    }
    return true;
  }

  /** @return the elements of this array as a java array. */
  public Object[] getItems() {
    return items;
  }

  /**
   * Marks the object as array-like for Polyglot APIs.
   *
   * @return {@code true}
   */
  @ExportMessage
  public boolean hasArrayElements() {
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
  public Object readArrayElement(
      long index,
      @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Cached BranchProfile errProfile,
      @Cached BranchProfile hasWarningsProfile)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    if (index >= items.length || index < 0) {
      errProfile.enter();
      throw InvalidArrayIndexException.create(index);
    }

    var v = items[(int) index];
    if (this.hasWarnings(warnings)) {
      hasWarningsProfile.enter();
      Warning[] extracted = this.getWarnings(null, warnings);
      if (warnings.hasWarnings(v)) {
        v = warnings.removeWarnings(v);
      }
      return new WithWarnings(v, extracted);
    }
    return v;
  }

  public long length() {
    return items.length;
  }

  /** @return an empty array */
  @Builtin.Method(description = "Creates an empty Array", autoRegister = false)
  public static Array empty() {
    return allocate(0);
  }

  @Builtin.Method(name = "slice", description = "Returns a slice of this Array.")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  public final Object slice(long start, long end, InteropLibrary interop)
      throws UnsupportedMessageException {
    var slice = ArraySlice.createOrNull(this, start, length(), end);
    return slice == null ? this : slice;
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
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().array();
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
    for (int i = 0; i < items.length; i++) {
      if (warnings.hasWarnings(items[i])) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  boolean hasWarnings(@CachedLibrary(limit = "3") WarningsLibrary warnings) {
    if (withWarnings == null) {
      withWarnings = hasWarningElements(items, warnings);
    }
    return withWarnings;
  }

  @ExportMessage
  Warning[] getWarnings(Node location, @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    ArrayRope<Warning> ropeOfWarnings = new ArrayRope<>();
    for (int i = 0; i < items.length; i++) {
      if (warnings.hasWarnings(items[i])) {
        ropeOfWarnings = ropeOfWarnings.prepend(warnings.getWarnings(items[i], location));
      }
    }
    return ropeOfWarnings.toArray(Warning[]::new);
  }

  @ExportMessage
  Array removeWarnings(@CachedLibrary(limit = "3") WarningsLibrary warnings)
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
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().array();
  }
}
