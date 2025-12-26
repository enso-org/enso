package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

@ExportLibrary(InteropLibrary.class)
final class VariableNodeObject implements TruffleObject {
  final String key;
  final String value;

  VariableNodeObject(String key, String value) {
    this.key = key;
    this.value = value;
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) {
    return ArrayLikeHelpers.wrapStrings(key);
  }

  @ExportMessage
  Object readMember(String id) throws UnknownIdentifierException {
    if (id.equals(key)) {
      return value;
    }
    throw UnknownIdentifierException.create(id);
  }

  @ExportMessage
  boolean isMemberReadable(String member) {
    return true;
  }
}
