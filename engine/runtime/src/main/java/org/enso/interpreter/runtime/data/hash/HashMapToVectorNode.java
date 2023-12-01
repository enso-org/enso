package org.enso.interpreter.runtime.data.hash;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;

@BuiltinMethod(
    type = "Map",
    name = "to_vector",
    description = """
        Transforms the hash map into a vector of key value pairs. If possible, caches
        the result. Key value pairs are represented as nested 2 element vectors.
        """,
    autoRegister = false
)
@GenerateUncached
public abstract class HashMapToVectorNode extends Node {

  public static HashMapToVectorNode build() {
    return HashMapToVectorNodeGen.create();
  }

  public abstract Object execute(Object self);

  @Specialization
  Object ensoMapToVector(EnsoHashMap hashMap,
      @Cached ConditionProfile vectorReprNotCachedProfile) {
    return hashMap.getCachedVectorRepresentation(vectorReprNotCachedProfile);
  }

  @Specialization(guards = "mapInterop.hasHashEntries(hashMap)", limit = "3")
  Object foreignMapToVector(Object hashMap,
      @CachedLibrary("hashMap") InteropLibrary mapInterop,
      @CachedLibrary(limit = "3") InteropLibrary iteratorInterop) {
    return createEntriesVectorFromForeignMap(hashMap, mapInterop, iteratorInterop);
  }

  @Fallback
  Object fallback(Object object) {
    return ArrayLikeHelpers.asVectorWithCheckAt(HashEntriesVector.createEmpty());
  }

  private static Object createEntriesVectorFromForeignMap(
      Object hashMap,
      InteropLibrary mapInterop,
      InteropLibrary iteratorInterop) {
    try {
      int hashSize = (int) mapInterop.getHashSize(hashMap);
      Object[] keys = new Object[hashSize];
      Object[] values = new Object[hashSize];
      Object entryIterator = mapInterop.getHashEntriesIterator(hashMap);
      int arrIdx = 0;
      while (iteratorInterop.hasIteratorNextElement(entryIterator)) {
        Object keyValueArr = iteratorInterop.getIteratorNextElement(entryIterator);
        keys[arrIdx] = iteratorInterop.readArrayElement(keyValueArr, 0);
        values[arrIdx] = iteratorInterop.readArrayElement(keyValueArr, 1);
        arrIdx++;
      }
      return ArrayLikeHelpers.asVectorFromArray(
          HashEntriesVector.createFromKeysAndValues(keys, values)
      );
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      var msg = "hashMap: " + hashMap + " has probably wrong hash interop API";
      throw new PanicException(Text.create(msg), mapInterop);
    }
  }

}
