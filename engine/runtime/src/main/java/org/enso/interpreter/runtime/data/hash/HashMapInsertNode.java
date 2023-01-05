package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;

@BuiltinMethod(
    type = "Hash_Map",
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

  abstract Object execute(Object hashMap, Object key, Object value);

  @Specialization
  Object doEnsoHashMap(EnsoHashMap hashMap, Object key, Object value) {
    hashMap.getMapBuilder().add(key, value);
    return hashMap.getMapBuilder().build();
  }

  @Specialization(guards = "mapInterop.hasHashEntries(foreignMap)", limit = "3")
  Object doForeign(Object foreignMap, Object keyToInsert, Object valueToInsert,
      @CachedLibrary("foreignMap") InteropLibrary mapInterop,
      @CachedLibrary(limit = "3") InteropLibrary iteratorInterop,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached EqualsAnyNode equalsNode) {
    var mapBuilder = EnsoHashMapBuilder.create(hashCodeNode, equalsNode);
    try {
      Object entriesIterator = mapInterop.getHashEntriesIterator(foreignMap);
      while (iteratorInterop.hasIteratorNextElement(entriesIterator)) {
        Object key = iteratorInterop.getIteratorNextElement(entriesIterator);
        Object value = iteratorInterop.getIteratorNextElement(entriesIterator);
        mapBuilder.add(key, value);
      }
    } catch (UnsupportedMessageException | StopIterationException e) {
      throw new IllegalStateException(
          "Polyglot hash map " + foreignMap + " has wrongly specified Interop API (hash entries iterator)",
          e
      );
    }
    mapBuilder.add(keyToInsert, valueToInsert);
    return EnsoHashMap.createWithBuilder(mapBuilder, mapBuilder.getSize());
  }
}
