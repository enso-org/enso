package org.enso.interpreter.epb;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.Arrays;

/** A node responsible for performing foreign JS calls. */
@NodeField(name = "foreignFunction", type = Object.class)
abstract class JsForeignNode extends ForeignFunctionCallNode {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();

  abstract Object getForeignFunction();

  /**
   * Creates a new instance of this node.
   *
   * @param jsFunction the parsed JS object (required to be {@link
   *     InteropLibrary#isExecutable(Object)})
   * @return a node able to call the JS function with given arguments
   */
  static JsForeignNode build(Object jsFunction) {
    return JsForeignNodeGen.create(jsFunction);
  }

  @Specialization
  Object doExecute(Object[] arguments, @CachedLibrary("foreignFunction") InteropLibrary iop)
      throws InteropException {
    var args = new ArgumentsArray(arguments);
    var self = arguments[0];
    var raw = iop.invokeMember(getForeignFunction(), "apply", self, args);
    return coercePrimitiveNode.execute(raw);
  }

  @ExportLibrary(InteropLibrary.class)
  static final class ArgumentsArray implements TruffleObject {
    private static final int OFFSET = 1;
    private final Object[] items;

    public ArgumentsArray(Object... items) {
      this.items = items;
    }

    @ExportMessage
    public boolean hasArrayElements() {
      return true;
    }

    @ExportMessage
    public Object readArrayElement(long index) throws InvalidArrayIndexException {
      if (index >= getArraySize() || index < 0) {
        throw InvalidArrayIndexException.create(index);
      }
      return items[OFFSET + (int) index];
    }

    @ExportMessage
    long getArraySize() {
      return items.length - OFFSET;
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
      return index < getArraySize() && index >= 0;
    }

    @ExportMessage
    void writeArrayElement(long index, Object value) throws UnsupportedMessageException {
      throw UnsupportedMessageException.create();
    }

    @ExportMessage
    boolean isArrayElementModifiable(long index) {
      return false;
    }

    @ExportMessage
    boolean isArrayElementInsertable(long index) {
      return false;
    }

    @Override
    public String toString() {
      return Arrays.toString(items);
    }
  }
}
