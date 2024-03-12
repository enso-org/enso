package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedSizeArrayBuilder implements TruffleObject {
  private final ByteBufferDirect buffer;
  private final LogicalLayout unit;
  private final int size;
  private int index;
  private boolean sealed;

  public ArrowFixedSizeArrayBuilder(int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = ByteBufferDirect.forSize(size, unit);
    this.index = 0;
    this.sealed = false;
  }

  public LogicalLayout getUnit() {
    return unit;
  }

  public boolean isSealed() {
    return sealed;
  }

  public ByteBufferDirect getBuffer() {
    return buffer;
  }

  public int getSize() {
    return size;
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return false;
  }

  @ExportMessage
  public boolean hasMembers() {
    return true;
  }

  @ExportMessage
  public boolean isMemberInvocable(String member) {
    return switch (member) {
      case "append" -> !this.sealed;
      case "build" -> true;
      default -> false;
    };
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
    return this;
  }

  @ExportMessage
  Object invokeMember(
      String name, Object[] args, @Cached(value = "build()") WriteToBuilderNode writeToBuilderNode)
      throws UnsupportedMessageException, UnknownIdentifierException, UnsupportedTypeException {
    switch (name) {
      case "build":
        sealed = true;
        return switch (unit) {
          case Date32, Date64 -> new ArrowFixedArrayDate(buffer, size, unit);
          case Int8, Int16, Int32, Int64 -> new ArrowFixedArrayInt(buffer, size, unit);
        };
      case "append":
        if (sealed) {
          throw UnsupportedMessageException.create();
        }
        var current = index;
        writeToBuilderNode.executeWrite(this, current, args[0]);
        index += 1;
        return NullValue.get();
      default:
        throw UnknownIdentifierException.create(name);
    }
  }

  @ExportMessage
  void writeArrayElement(long index, Object value)
      throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {}

  @ExportMessage
  long getArraySize() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  Object readArrayElement(long index)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return false;
  }
}
