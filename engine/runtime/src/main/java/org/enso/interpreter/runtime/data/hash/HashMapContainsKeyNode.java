package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Map",
    name = "contains_key",
    description =
        """
        Returns True if the hash map contains mapping with the given key, False otherwise.
        """,
    autoRegister = false)
@GenerateUncached
public abstract class HashMapContainsKeyNode extends Node {

  public static HashMapContainsKeyNode build() {
    return HashMapContainsKeyNodeGen.create();
  }

  public abstract boolean execute(Object self, Object key);

  @Specialization(
      guards = {"interop.hasHashEntries(foreignMap)"},
      limit = "3")
  boolean doForeignHashMap(
      Object foreignMap, Object key, @CachedLibrary("foreignMap") InteropLibrary interop) {
    return interop.isHashEntryExisting(foreignMap, key);
  }

  @Fallback
  boolean fallback(Object map, Object key) {
    return false;
  }
}
