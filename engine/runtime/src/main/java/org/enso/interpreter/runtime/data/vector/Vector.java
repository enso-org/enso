package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.EconomicSet;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Data.Vector.Vector")
public abstract class Vector implements EnsoObject {
  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return true;
  }

  @ExportMessage
  long getArraySize() {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  Object readArrayElement(long index)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  final void writeArrayElement(long index, Object value) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementRemovable(long index) {
    return false;
  }

  @ExportMessage
  final void removeArrayElement(long index) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  String toDisplayString(boolean allowSideEffects) {
    final InteropLibrary iop = InteropLibrary.getUncached();
    return DisplayArrayUtils.toDisplayString(this, allowSideEffects, iop);
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  //
  // methods for TypesLibrary
  //

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  //
  // helper methods
  //

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return toDisplayString(false);
  }

  static Vector fromInteropArray(Object arr) {
    return new Generic(arr);
  }

  static Vector fromLongArray(long[] arr) {
    return new Long(arr);
  }

  static Vector fromDoubleArray(double[] arr) {
    return new Double(arr);
  }

  static Object fromEnsoOnlyArray(Object[] arr) {
    return new EnsoOnly(arr);
  }

  @ExportLibrary(InteropLibrary.class)
  @ExportLibrary(WarningsLibrary.class)
  public static final class EnsoOnly extends Vector {
    private final Object[] storage;

    private EnsoOnly(Object[] storage) {
      this.storage = storage;
    }

    //
    // messages for the InteropLibrary
    //

    @ExportMessage
    long getArraySize() {
      return storage.length;
    }

    @ExportMessage
    Object readArrayElement(long index) throws InvalidArrayIndexException {
      try {
        return storage[Math.toIntExact(index)];
      } catch (ArithmeticException | IndexOutOfBoundsException ex) {
        throw InvalidArrayIndexException.create(index);
      }
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
      var size = storage.length;
      return index < size && index >= 0;
    }

    @ExportMessage
    boolean hasWarnings() {
      return false;
    }

    @ExportMessage
    Warning[] getWarnings(Node location) throws UnsupportedMessageException {
      return new Warning[0];
    }

    @ExportMessage
    Warning[] getElementWarnings(
        Node location)
        throws UnsupportedMessageException {
      return new Warning[0];
    }

    @ExportMessage
    EnsoOnly removeWarnings() throws UnsupportedMessageException {
      return this;
    }

    @ExportMessage
    boolean isLimitReached() {
      return false;
    }
  }

  @ExportLibrary(InteropLibrary.class)
  @ExportLibrary(WarningsLibrary.class)
  public static final class Generic extends Vector {
    private final Object storage;

    private Generic(Object storage) {
      if (CompilerDirectives.inInterpreter()) {
        if (!InteropLibrary.getUncached().hasArrayElements(storage)) {
          throw EnsoContext.get(null)
              .raiseAssertionPanic(
                  null, "Vector needs array-like delegate, but got: " + storage, null);
        }
      }
      this.storage = storage;
    }

    final Object toArray() {
      return this.storage;
    }

    //
    // messages for the InteropLibrary
    //

    @ExportMessage
    long getArraySize(
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop)
        throws UnsupportedMessageException {
      return interop.getArraySize(storage);
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
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop,
        @CachedLibrary(limit = "3") WarningsLibrary warnings,
        @Cached HostValueToEnsoNode toEnso)
        throws InvalidArrayIndexException, UnsupportedMessageException {
      var v = interop.readArrayElement(this.storage, index);
      //System.out.println("AAA rae " + index + " " + v + " " + warnings.hasWarnings(this.storage));
      if (warnings.hasWarnings(this.storage)) {
        Warning[] extracted = warnings.getWarnings(this.storage, null);
        //System.out.println("AAA rae ws " + extracted + " " + extracted.length);
        for (Warning warning : extracted) {
         //System.out.println("AAA rae w " + warning);
        }
        if (warnings.hasWarnings(v)) {
          //System.out.println("AAA rae e orig " + warnings.getWarnings(v, null));
          for (Warning warning : warnings.getWarnings(v, null)) {
            //System.out.println("AAA rae e w " + warning);
          }
          v = warnings.removeWarnings(v);
        }
        return WithWarnings.wrap(EnsoContext.get(interop), toEnso.execute(v), extracted);
      }
      return toEnso.execute(v);
    }

    /**
     * Exposes an index validity check through the polyglot API.
     *
     * @param index the index to check
     * @return {@code true} if the index is valid, {@code false} otherwise.
     */
    @ExportMessage
    boolean isArrayElementReadable(
        long index,
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
      try {
        var size = interop.getArraySize(storage);
        return index < size && index >= 0;
      } catch (UnsupportedMessageException e) {
        return false;
      }
    }

    @ExportMessage
    boolean hasWarnings(
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
      return warnings.hasWarnings(this.storage);
    }

    @ExportMessage
    Warning[] getWarnings(
        Node location,
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
        throws UnsupportedMessageException {
      return warnings.getWarnings(this.storage, location);
    }

    @ExportMessage
    Warning[] getElementWarnings(
        Node location,
        /*@Cached.Shared(value = "interop")*/ @CachedLibrary(limit = "3") InteropLibrary interop,
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
        throws UnsupportedMessageException {
      EnsoContext ctx = EnsoContext.get(warnings);
      EconomicSet<Warning> setOfWarnings = EconomicSet.create(new WithWarnings.WarningEquivalence());
      for (int index = 0; index < getArraySize(interop); ++index) {
        try {
          final int finalIndex = index;
          //var v = interop.readArrayElement(this.storage, index);
          var v = ((Array)this.storage).readArrayElementNoWarningPropagation(index);
          //System.out.println("AAA gew " + index + " " + v.getClass() + " " + v);
          if (warnings.hasWarnings(v)) {
            Warning elementWarnings[] = warnings.getWarnings(v, location);
            Warning wrapped[] = Arrays.stream(elementWarnings).map(warning -> {
              var error = warning.getValue();
              var wrappedError = ctx.getBuiltins().error().makeMapError(finalIndex, error);
              var wrappedWarning = Warning.create(ctx, wrappedError, warning.getOrigin());
              return wrappedWarning;
            }).toArray(Warning[]::new);
            setOfWarnings.addAll(Arrays.asList(wrapped));
          }
        } catch (InvalidArrayIndexException e) {
          throw ctx.raiseAssertionPanic(warnings, null, e);
        }
      }
      return Warning.fromSetToArray(setOfWarnings);
    }

    @ExportMessage
    Generic removeWarnings(
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
        throws UnsupportedMessageException {
      return new Generic(warnings.removeWarnings(this.storage));
    }

    @ExportMessage
    boolean isLimitReached(
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
      return warnings.isLimitReached(this.storage);
    }
  }

  @ExportLibrary(value = InteropLibrary.class)
  @ExportLibrary(value = WarningsLibrary.class)
  public static final class Double extends Vector {
    private final double[] storage;

    private Double(double[] storage) {
      this.storage = storage;
    }

    @ExportMessage
    long getArraySize() {
      return storage.length;
    }

    /**
     * Handles reading an element by index through the polyglot API.
     *
     * @param index the index to read
     * @return the element value at the provided index
     * @throws InvalidArrayIndexException when the index is out of bounds.
     */
    @ExportMessage
    Object readArrayElement(long index) throws InvalidArrayIndexException {
      try {
        return storage[Math.toIntExact(index)];
      } catch (ArithmeticException | IndexOutOfBoundsException ex) {
        throw InvalidArrayIndexException.create(index);
      }
    }

    /**
     * Exposes an index validity check through the polyglot API.
     *
     * @param index the index to check
     * @return {@code true} if the index is valid, {@code false} otherwise.
     */
    @ExportMessage
    boolean isArrayElementReadable(long index) {
      var size = storage.length;
      return index < size && index >= 0;
    }

    @ExportMessage
    boolean hasWarnings() {
      return false;
    }

    @ExportMessage
    Warning[] getWarnings(Node location) throws UnsupportedMessageException {
      return new Warning[0];
    }

    @ExportMessage
    Warning[] getElementWarnings(
        Node location)
        throws UnsupportedMessageException {
      return new Warning[0];
    }

    @ExportMessage
    Double removeWarnings() {
      return this;
    }

    @ExportMessage
    boolean isLimitReached() {
      return false;
    }
  }

  @ExportLibrary(value = InteropLibrary.class)
  @ExportLibrary(value = WarningsLibrary.class)
  public static final class Long extends Vector {
    private final long[] storage;

    private Long(long[] storage) {
      this.storage = storage;
    }

    @ExportMessage
    long getArraySize() {
      return storage.length;
    }

    @ExportMessage
    Object readArrayElement(long index) throws InvalidArrayIndexException {
      try {
        return storage[Math.toIntExact(index)];
      } catch (ArithmeticException | IndexOutOfBoundsException ex) {
        throw InvalidArrayIndexException.create(index);
      }
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
      var size = storage.length;
      return index < size && index >= 0;
    }

    @ExportMessage
    boolean hasWarnings() {
      return false;
    }

    @ExportMessage
    Warning[] getWarnings(Node location) throws UnsupportedMessageException {
      return new Warning[0];
    }

    @ExportMessage
    Warning[] getElementWarnings(
        Node location)
        throws UnsupportedMessageException {
      return new Warning[0];
    }

    @ExportMessage
    Long removeWarnings() {
      return this;
    }

    @ExportMessage
    boolean isLimitReached() {
      return false;
    }
  }
}
