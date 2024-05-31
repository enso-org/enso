package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.WarningsLibrary;

@ExportLibrary(InteropLibrary.class)
final class ArrayBuilder implements EnsoObject {
  private static final String[] MEMBERS =
      new String[] {"isEmpty", "add", "appendTo", "get", "getSize", "toArray"};
  private final int initialCapacity;
  private int size;
  private Object primitiveArray;
  private Object[] objectArray;

  /** becomes {@code true} when a non trivial value is being added to the builder */
  private boolean nonTrivialEnsoValue;

  private ArrayBuilder(int initialCapacity) {
    this.initialCapacity = Math.max(1, initialCapacity);
  }

  /** Creates new builder */
  static ArrayBuilder newBuilder(int capacity) {
    return new ArrayBuilder(capacity);
  }

  /** Is the builder empty? */
  boolean isEmpty() {
    return size == 0;
  }

  /**
   * Adds an element to the builder
   *
   * @param e the element to add
   * @param warnings library to check for values with warnings
   */
  void add(Object e, WarningsLibrary warnings) {
    if (!nonTrivialEnsoValue) {
      if (warnings.hasWarnings(e)) {
        nonTrivialEnsoValue = true;
      } else {
        var isEnsoValue = e instanceof EnsoObject || e instanceof Long || e instanceof Double;
        if (!isEnsoValue) {
          nonTrivialEnsoValue = true;
        }
      }
    }
    addImpl(e);
  }

  private void addImpl(Object e) {
    while (true) {
      // loop at most twice thanks to continue and break
      if (objectArray != null) {
        if (size == objectArray.length) {
          CompilerDirectives.transferToInterpreter();
          objectArray = Arrays.copyOf(objectArray, size * 2);
        }
        objectArray[size++] = e;
      } else if (primitiveArray instanceof long[] longArray) {
        if (e instanceof Long l) {
          if (size == longArray.length) {
            CompilerDirectives.transferToInterpreter();
            primitiveArray = longArray = Arrays.copyOf(longArray, size * 2);
          }
          longArray[size++] = l;
        } else {
          CompilerDirectives.transferToInterpreter();
          objectArray = new Object[longArray.length];
          for (int i = 0; i < size; i++) {
            objectArray[i] = longArray[i];
          }
          primitiveArray = null;
          continue;
        }
      } else if (primitiveArray instanceof double[] doubleArray) {
        if (e instanceof Double d) {
          if (size == doubleArray.length) {
            CompilerDirectives.transferToInterpreter();
            primitiveArray = doubleArray = Arrays.copyOf(doubleArray, size * 2);
          }
          doubleArray[size++] = d;
        } else {
          CompilerDirectives.transferToInterpreter();
          objectArray = new Object[doubleArray.length];
          for (int i = 0; i < size; i++) {
            objectArray[i] = doubleArray[i];
          }
          primitiveArray = null;
          continue;
        }
      } else {
        assert objectArray == null;
        assert primitiveArray == null;
        assert size == 0;
        switch (e) {
          case Long l -> {
            var arr = new long[initialCapacity];
            arr[0] = l;
            primitiveArray = arr;
          }
          case Double d -> {
            var arr = new double[initialCapacity];
            arr[0] = d;
            primitiveArray = arr;
          }
          default -> {
            var arr = new Object[initialCapacity];
            arr[0] = e;
            objectArray = arr;
          }
        }
        size = 1;
      }
      break;
    }
  }

  /** Obtains an element from the builder */
  private Object get(int index, Node node) {
    try {
      if (index >= 0 && index < size) {
        if (objectArray != null) {
          return objectArray[index];
        } else if (primitiveArray instanceof long[] longArray) {
          return longArray[index];
        } else if (primitiveArray instanceof double[] doubleArray) {
          return doubleArray[index];
        }
      }
      throw new ArrayIndexOutOfBoundsException();
    } catch (IndexOutOfBoundsException e) {
      throw ArrayPanics.indexOutOfBounds(node, index, size);
    }
  }

  /** Returns the current array of the builder. */
  private Object toArray() {
    if (objectArray != null) {
      return objectArray.length == size ? objectArray : Arrays.copyOf(objectArray, size);
    } else if (primitiveArray instanceof long[] longArray) {
      return longArray.length == size ? longArray : Arrays.copyOf(longArray, size);
    } else if (primitiveArray instanceof double[] doubleArray) {
      return doubleArray.length == size ? doubleArray : Arrays.copyOf(doubleArray, size);
    } else {
      return null;
    }
  }

  int getSize() {
    return size;
  }

  @ExportMessage
  Object invokeMember(
      String name,
      Object[] args,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnknownIdentifierException, UnsupportedTypeException, UnsupportedMessageException {
    return switch (name) {
      case "isEmpty" -> isEmpty();
      case "add" -> {
        add(args[0], warnings);
        yield this;
      }
      case "appendTo" -> {
        long len = iop.getArraySize(args[0]);
        for (var i = 0; i < len; i++) {
          try {
            var e = iop.readArrayElement(args[0], i);
            add(e, warnings);
          } catch (InvalidArrayIndexException ex) {
            throw UnsupportedTypeException.create(args);
          }
        }
        yield this;
      }
      case "get" -> {
        if (!iop.isNumber(args[0])) {
          throw UnsupportedTypeException.create(args);
        }
        if (!iop.fitsInInt(args[0])) {
          throw UnsupportedTypeException.create(args);
        }
        var index = iop.asInt(args[0]);
        yield get(index, iop);
      }
      case "getSize" -> getSize();
      case "toArray" -> asVector();
      default -> throw UnknownIdentifierException.create(name);
    };
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  @ExportMessage
  boolean isMemberInvocable(String member) {
    for (var m : MEMBERS) {
      if (m.equals(member)) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  EnsoObject getMembers(boolean includeInternal) {
    return ArrayLikeHelpers.wrapStrings(MEMBERS);
  }

  @ExportMessage
  String toDisplayString(boolean ignore) {
    return "Array_Builder";
  }

  Object asVector() {
    var res = toArray();
    if (res instanceof long[] longs) {
      return Vector.fromLongArray(longs);
    }
    if (res instanceof double[] doubles) {
      return Vector.fromDoubleArray(doubles);
    }
    if (nonTrivialEnsoValue) {
      return Vector.fromInteropArray(Array.wrap((Object[]) res));
    } else {
      return Vector.fromEnsoOnlyArray((Object[]) res);
    }
  }
}
