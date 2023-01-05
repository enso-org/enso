package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Vector;


@BuiltinMethod(
    type = "Hash_Map",
    name = "to_flat_vector",
    description = """
        Transforms the hash map into a flat vector of key value pairs. If possible, caches
        the result.
        """,
    autoRegister = false
)
@GenerateUncached
public abstract class HashMapToFlatVectorNode extends Node {

  public static HashMapToFlatVectorNode build() {
    return HashMapToFlatVectorNodeGen.create();
  }

  abstract Object execute(Object hashMap);

  @Specialization(limit = "3")
  Object foreignWrapperToFlatVec(ForeignMapWrapper mapWrapper,
      @CachedLibrary("mapWrapper") InteropLibrary mapInterop,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached ConditionProfile isFlatVectorNotCached) {
    Object cachedVector = mapWrapper.getCachedVectorRepresentation();
    if (isFlatVectorNotCached.profile(cachedVector == null)) {
      cachedVector = createFlatVectorFromForeignMap(mapWrapper, mapInterop, interop);
      mapWrapper.setCachedVectorRepresentation(cachedVector);
    }
    return cachedVector;
  }

  @Specialization
  Object ensoHashMapToFlatVec(EnsoHashMap hashMap,
      @Cached ConditionProfile vectorReprNotCached) {
    if (vectorReprNotCached.profile(!hashMap.isVectorRepresentationCached())) {
      hashMap.cacheVectorRepresentation();
    }
    return hashMap.getCachedVectorRepresentation();
  }

  @Specialization(guards = "mapInterop.hasHashEntries(hashMap)", limit = "3")
  Object interopMapToFlatVec(Object hashMap,
      @CachedLibrary("hashMap") InteropLibrary mapInterop,
      @CachedLibrary(limit = "3") InteropLibrary iteratorInterop) {
    return createFlatVectorFromForeignMap(hashMap, mapInterop, iteratorInterop);
  }

  @Fallback
  Object fallback(Object object) {
    return Vector.fromArray(new FlatKeyValueVector(new Object[]{}));
  }

  private static Object createFlatVectorFromForeignMap(
      Object hashMap,
      InteropLibrary mapInterop,
      InteropLibrary iteratorInterop) {
    try {
      Object[] vectorContent = new Object[(int) mapInterop.getHashSize(hashMap) * 2];
      Object entryIterator = mapInterop.getHashEntriesIterator(hashMap);
      int arrIdx = 0;
      while (iteratorInterop.hasIteratorNextElement(entryIterator)) {
        Object key = iteratorInterop.getIteratorNextElement(entryIterator);
        Object value = iteratorInterop.getIteratorNextElement(entryIterator);
        vectorContent[arrIdx++] = key;
        vectorContent[arrIdx++] = value;
      }
      return Vector.fromArray(
          new FlatKeyValueVector(vectorContent)
      );
    } catch (UnsupportedMessageException | StopIterationException e) {
      throw new IllegalStateException("hashMap: " + hashMap + " has probably wrong interop API", e);
    }
  }

}
