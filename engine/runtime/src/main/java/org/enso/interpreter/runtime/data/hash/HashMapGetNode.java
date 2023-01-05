package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Hash_Map",
    name = "get",
    description = """
        Gets a value from the map on the specified key, or default.
        """,
    autoRegister = false
)
@GenerateUncached
public abstract class HashMapGetNode extends Node {

  public static HashMapGetNode build() {
    return HashMapGetNodeGen.create();
  }

  abstract Object execute(Object hashMap, Object key, Object defaultValue);

  @Specialization(guards = "interop.hasHashEntries(hashMap)", limit = "3")
  Object hashMapGet(Object hashMap, Object key, Object defaultValue,
      @CachedLibrary("hashMap") InteropLibrary interop) {
    if (interop.isHashEntryReadable(hashMap, key)) {
      try {
        return interop.readHashValue(hashMap, key);
      } catch (UnsupportedMessageException | UnknownKeyException e) {
        throw new IllegalStateException(e);
      }
    } else {
      return defaultValue;
    }
  }

  @Fallback
  Object fallback(Object hashMap, Object key, Object defaultValue) {
    return defaultValue;
  }
}
