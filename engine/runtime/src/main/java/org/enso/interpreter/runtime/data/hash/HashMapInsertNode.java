package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;

@BuiltinMethod(
    type = "Map",
    name = "insert",
    description = """
        Returns newly created hash map with the given key value mapping.
        """,
    autoRegister = false
)
public abstract class HashMapInsertNode extends Node {

  public static HashMapInsertNode build() {
    return HashMapInsertNodeGen.create();
  }

  public abstract EnsoHashMap execute(Object self, Object key, Object value);

  @Specialization
  @TruffleBoundary
  EnsoHashMap doEnsoHashMap(EnsoHashMap hashMap, Object key, Object value) {
    EnsoHashMapBuilder mapBuilder = hashMap.getMapBuilder();
    boolean containsKey = mapBuilder.get(key) != null;
    boolean insertCalledOnMap = hashMap.isInsertCalled();
    if (insertCalledOnMap || containsKey) {
      // insert was already called on this map => We need to duplicate MapBuilder
      // If a key is already contained in the Map there is no way telling whether there is another
      // binding pointing to the Map, and we do not want to mutate this older binding.
      var newMapBuilder = hashMap.getHashSize() < mapBuilder.getSize() ?
          mapBuilder.duplicatePartial(hashMap.getHashSize()) :
          mapBuilder.duplicate();
      newMapBuilder.add(key, value);
      return newMapBuilder.build();
    } else {
      // Do not duplicate the builder, just create a snapshot.
      mapBuilder.add(key, value);
      var newMap = mapBuilder.build();
      hashMap.setInsertCalled();
      return newMap;
    }
  }

  /**
   * Creates a new {@link EnsoHashMapBuilder} for the given {@code foreignMap} - iterates through
   * all the entries of the foreign map. The returned map is {@link EnsoHashMap}.
   */
  @Specialization(guards = "mapInterop.hasHashEntries(foreignMap)", limit = "3")
  EnsoHashMap doForeign(Object foreignMap, Object keyToInsert, Object valueToInsert,
      @CachedLibrary("foreignMap") InteropLibrary mapInterop,
      @CachedLibrary(limit = "3") InteropLibrary iteratorInterop,
      @Cached HashCodeNode hashCodeNode,
      @Cached EqualsNode equalsNode) {
    var mapBuilder = EnsoHashMapBuilder.create(hashCodeNode, equalsNode);
    try {
      Object entriesIterator = mapInterop.getHashEntriesIterator(foreignMap);
      while (iteratorInterop.hasIteratorNextElement(entriesIterator)) {
        Object keyValueArr = iteratorInterop.getIteratorNextElement(entriesIterator);
        Object key = iteratorInterop.readArrayElement(keyValueArr, 0);
        Object value = iteratorInterop.readArrayElement(keyValueArr, 1);
        mapBuilder.add(key, value);
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      throw new IllegalStateException(
          "Polyglot hash map " + foreignMap + " has wrongly specified Interop API (hash entries iterator)",
          e
      );
    }
    mapBuilder.add(keyToInsert, valueToInsert);
    return EnsoHashMap.createWithBuilder(mapBuilder, mapBuilder.getSize());
  }
}
