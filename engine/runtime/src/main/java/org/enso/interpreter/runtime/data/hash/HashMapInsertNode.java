package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
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
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Map",
    name = "insert",
    description =
        """
        Returns newly created hash map with the given key value mapping.
        """,
    autoRegister = false)
public abstract class HashMapInsertNode extends Node {

  public static HashMapInsertNode build() {
    return HashMapInsertNodeGen.create();
  }

  public abstract EnsoHashMap execute(Object self, Object key, Object value);

  @Specialization
  EnsoHashMap doEnsoHashMap(
      EnsoHashMap hashMap,
      Object key,
      Object value,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode) {
    var mapBuilder = hashMap.getMapBuilder(false, hashCodeNode, equalsNode);
    mapBuilder.put(key, value, hashCodeNode, equalsNode);
    var newMap = mapBuilder.build();
    return newMap;
  }

  /**
   * Creates a new {@link EnsoHashMapBuilder} for the given {@code foreignMap} - iterates through
   * all the entries of the foreign map. The returned map is {@link EnsoHashMap}.
   */
  @Specialization(guards = "mapInterop.hasHashEntries(foreignMap)", limit = "3")
  EnsoHashMap doForeign(
      Object foreignMap,
      Object keyToInsert,
      Object valueToInsert,
      @CachedLibrary("foreignMap") InteropLibrary mapInterop,
      @CachedLibrary(limit = "3") InteropLibrary iteratorInterop,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode) {
    var mapBuilder = EnsoHashMapBuilder.create();
    try {
      Object entriesIterator = mapInterop.getHashEntriesIterator(foreignMap);
      while (iteratorInterop.hasIteratorNextElement(entriesIterator)) {
        Object keyValueArr = iteratorInterop.getIteratorNextElement(entriesIterator);
        Object key = iteratorInterop.readArrayElement(keyValueArr, 0);
        Object value = iteratorInterop.readArrayElement(keyValueArr, 1);
        mapBuilder = mapBuilder.asModifiable(mapBuilder.generation(), hashCodeNode, equalsNode);
        mapBuilder.put(key, value, hashCodeNode, equalsNode);
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      var msg =
          "Polyglot hash map "
              + foreignMap
              + " has wrongly specified Interop API (hash entries iterator)";
      throw new PanicException(Text.create(msg), this);
    }
    mapBuilder = mapBuilder.asModifiable(mapBuilder.generation(), hashCodeNode, equalsNode);
    mapBuilder.put(keyToInsert, valueToInsert, hashCodeNode, equalsNode);
    return EnsoHashMap.createWithBuilder(mapBuilder);
  }
}
