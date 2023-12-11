package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Map",
    name = "size",
    description = "Returns the number of entries in this hash map",
    autoRegister = false)
@GenerateUncached
public abstract class HashMapSizeNode extends Node {

  public static HashMapSizeNode build() {
    return HashMapSizeNodeGen.create();
  }

  public abstract long execute(Object self);

  @Specialization(guards = "interop.hasHashEntries(hashMap)", limit = "3")
  long getHashMapSize(Object hashMap, @CachedLibrary("hashMap") InteropLibrary interop) {
    try {
      return interop.getHashSize(hashMap);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(Text.create(e.getMessage()), this);
    }
  }

  @Fallback
  long fallback(Object hashMap) {
    return 0;
  }
}
