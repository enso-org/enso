package org.enso.interpreter.runtime.data.hash;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;

@BuiltinMethod(
    type = "Map",
    name = "to_text",
    description = """
        Returns text representation of this hash map
        """,
    autoRegister = false
)
public abstract class HashMapToTextNode extends Node {

  public static HashMapToTextNode build() {
    return HashMapToTextNodeGen.create();
  }

  public abstract Object execute(Object self);

  @TruffleBoundary
  @Specialization(guards = "interop.hasHashEntries(hashMap)")
  Object hashMapToText(Object hashMap,
      @CachedLibrary(limit = "5") InteropLibrary interop) {
    var sb = new StringBuilder();
    sb.append("{");
    try {
      Object entryIterator = interop.getHashEntriesIterator(hashMap);
      while (interop.hasIteratorNextElement(entryIterator)) {
        Object keyValuePair = interop.getIteratorNextElement(entryIterator);
        Object key = interop.readArrayElement(keyValuePair, 0);
        Object value = interop.readArrayElement(keyValuePair, 1);
        sb.append(key).append("=").append(value).append(", ");
      }
      if (interop.getHashSize(hashMap) > 0) {
        // Delete last comma
        sb.delete(sb.length() - 2, sb.length());
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      var ctx = EnsoContext.get(this);
      var msg = "hashMap " + hashMap + " probably implements interop API incorrectly";
      throw ctx.raiseAssertionPanic(this, msg, e);
    }
    sb.append("}");
    return sb.toString();
  }
}

