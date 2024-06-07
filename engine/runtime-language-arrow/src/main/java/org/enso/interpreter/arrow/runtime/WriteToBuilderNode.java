package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.InlinedExactClassProfile;
import org.enso.interpreter.arrow.LogicalLayout;

@ImportStatic(LogicalLayout.class)
@GenerateUncached
@GenerateInline(value = false)
abstract class WriteToBuilderNode extends Node {

  public abstract void executeWrite(ArrowFixedSizeArrayBuilder receiver, long index, Object value)
      throws UnsupportedTypeException, UnsupportedMessageException;

  @NeverDefault
  static WriteToBuilderNode build() {
    return WriteToBuilderNodeGen.create();
  }

  @NeverDefault
  static WriteToBuilderNode getUncached() {
    return WriteToBuilderNodeGen.getUncached();
  }

  @Specialization(
      limit = "3",
      guards = {"receiver.getUnit() == cachedUnit"})
  static void doWriteToIndex(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      Object value,
      @Bind("$node") Node node,
      @Cached(value = "receiver.getUnit()", allowUncached = true) LogicalLayout cachedUnit,
      @CachedLibrary(limit = "1") InteropLibrary iop,
      @Cached ValueToNumberNode toNumber,
      @Cached InlinedExactClassProfile bufferClazz)
      throws UnsupportedTypeException, UnsupportedMessageException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
    } else {
      var number = toNumber.executeAdjust(receiver.getUnit(), value);
      var at = typeAdjustedIndex(index, cachedUnit);
      var buf = receiver.getBuffer();
      switch (number) {
        case Byte b -> buf.put(at, b.byteValue());
        case Short s -> buf.putShort(at, s.shortValue());
        case Integer i -> buf.putInt(at, i.intValue());
        case Long l -> buf.putLong(at, l.longValue());
        default -> throw UnsupportedTypeException.create(new Object[] {value}, "value is unknown");
      }
    }
  }

  @Specialization(replaces = "doWriteToIndex")
  void doWriteToIndexFallback(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      Object value,
      @Bind("$node") Node node,
      @CachedLibrary(limit = "1") InteropLibrary iop,
      @Cached ValueToNumberNode toNumber,
      @Cached InlinedExactClassProfile bufferClazz)
      throws UnsupportedTypeException, UnsupportedMessageException {

    doWriteToIndex(receiver, index, value, node, receiver.getUnit(), iop, toNumber, bufferClazz);
  }

  private static void validAccess(ArrowFixedSizeArrayBuilder receiver, long index)
      throws UnsupportedTypeException {
    if (receiver.isSealed()) {
      throw UnsupportedTypeException.create(
          new Object[] {receiver}, "receiver is not an unsealed buffer");
    }
    if (index >= receiver.getSize() || index < 0) {
      throw UnsupportedTypeException.create(new Object[] {index}, "index is out of range");
    }
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return ArrowFixedArrayDate.typeAdjustedIndex(index, unit.sizeInBytes());
  }
}
