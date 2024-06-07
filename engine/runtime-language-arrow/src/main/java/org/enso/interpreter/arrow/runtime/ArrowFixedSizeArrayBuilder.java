package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedSizeArrayBuilder implements TruffleObject {
  private final LogicalLayout unit;
  private final int size;
  private ByteBufferDirect buffer;

  private static final String APPEND_OP = "append";
  private static final String BUILD_OP = "build";

  public ArrowFixedSizeArrayBuilder(int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = ByteBufferDirect.forSize(size, unit);
  }

  public LogicalLayout getUnit() {
    return unit;
  }

  public boolean isSealed() {
    return buffer == null;
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
      case APPEND_OP -> buffer != null;
      case BUILD_OP -> true;
      default -> false;
    };
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
    return this;
  }

  @ExportMessage
  Object invokeMember(String name, Object[] args, @Cached AppendNode append)
      throws UnsupportedMessageException, UnknownIdentifierException, UnsupportedTypeException {
    return switch (name) {
      case BUILD_OP -> build();
      case APPEND_OP -> {
        append.executeAppend(this, args[0]);
        yield NullValue.get();
      }
      default -> throw UnknownIdentifierException.create(name);
    };
  }

  private final TruffleObject build() throws UnsupportedMessageException {
    var b = buffer;
    if (b == null) {
      throw UnsupportedMessageException.create();
    }
    buffer = null;
    return switch (unit) {
      case Date32, Date64 -> new ArrowFixedArrayDate(b, size, unit);
      case Int8, Int16, Int32, Int64 -> new ArrowFixedArrayInt(b, size, unit);
    };
  }

  @GenerateUncached
  @GenerateInline(false)
  abstract static class AppendNode extends Node {
    abstract void executeAppend(ArrowFixedSizeArrayBuilder builder, Object value)
        throws UnsupportedTypeException, UnsupportedMessageException;

    @Specialization(
        limit = "3",
        guards = {"builder.getUnit() == cachedUnit"})
    static void writeToBuffer(
        ArrowFixedSizeArrayBuilder builder,
        Object value,
        @Cached(value = "builder.getUnit()", allowUncached = true) LogicalLayout cachedUnit,
        @Cached ByteBufferDirect.PutNode put,
        @Cached ValueToNumberNode valueNode,
        @CachedLibrary(limit = "3") InteropLibrary iop)
        throws UnsupportedTypeException, UnsupportedMessageException {
      if (iop.isNull(value)) {
        put.putNull(builder.buffer, cachedUnit);
        return;
      }
      var number = valueNode.executeAdjust(cachedUnit, value);
      switch (number) {
        case Byte b -> put.put(builder.buffer, b);
        case Short s -> put.putShort(builder.buffer, s);
        case Integer i -> put.putInt(builder.buffer, i);
        case Long l -> put.putLong(builder.buffer, l);
        default -> throw CompilerDirectives.shouldNotReachHere();
      }
    }

    @Specialization(replaces = "writeToBuffer")
    static void writeToBufferUncached(
        ArrowFixedSizeArrayBuilder builder,
        Object value,
        @Cached ByteBufferDirect.PutNode put,
        @Cached ValueToNumberNode valueNode,
        @CachedLibrary(limit = "3") InteropLibrary iop)
        throws UnsupportedTypeException, UnsupportedMessageException {
      writeToBuffer(builder, value, builder.getUnit(), put, valueNode, iop);
    }
  }

  @GenerateUncached
  @GenerateInline(false)
  abstract static class BuildNode extends Node {
    abstract TruffleObject executeBuild(ArrowFixedSizeArrayBuilder builder)
        throws UnsupportedMessageException;

    @Specialization
    static TruffleObject buildIt(ArrowFixedSizeArrayBuilder builder)
        throws UnsupportedMessageException {
      return builder.build();
    }
  }
}
