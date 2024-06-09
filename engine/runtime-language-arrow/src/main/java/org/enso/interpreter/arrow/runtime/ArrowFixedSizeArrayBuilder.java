package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
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

  private static final String APPEND_OP = "append";
  private static final String BUILD_OP = "build";

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
  public boolean hasMembers() {
    return true;
  }

  @ExportMessage
  public boolean isMemberInvocable(String member) {
    return switch (member) {
      case APPEND_OP -> !this.sealed;
      case BUILD_OP -> true;
      default -> false;
    };
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
    return this;
  }

  @ExportMessage
  Object invokeMember(
      String name,
      Object[] args,
      @Cached(value = "buildWriterOrNull(name)", neverDefault = true)
          WriteToBuilderNode writeToBuilderNode)
      throws UnsupportedMessageException, UnknownIdentifierException, UnsupportedTypeException {
    switch (name) {
      case BUILD_OP:
        sealed = true;
        return switch (unit) {
          case Date32, Date64 -> new ArrowFixedArrayDate(buffer, size, unit);
          case Int8, Int16, Int32, Int64 -> new ArrowFixedArrayInt(buffer, size, unit);
        };
      case APPEND_OP:
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

  static WriteToBuilderNode buildWriterOrNull(String op) {
    return APPEND_OP.equals(op) ? WriteToBuilderNode.build() : WriteToBuilderNodeGen.getUncached();
  }
}
