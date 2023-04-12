package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
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
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Map",
    name = "remove_builtin",
    description = """
        Removes an entry from this map specified with the key.
        """
)
@GenerateUncached
public abstract class HashMapRemoveNode extends Node {
  public static HashMapRemoveNode build() {
    return HashMapRemoveNodeGen.create();
  }

  public abstract EnsoHashMap execute(Object self, Object key);

  @Specialization
  EnsoHashMap removeFromEnsoMap(EnsoHashMap ensoMap, Object key) {
    var oldEntry = ensoMap.getMapBuilder().get(key);
    if (oldEntry == null) {
      throw DataflowError.withoutTrace("No such key", null);
    } else {
      var newBuilder = ensoMap.getMapBuilder().duplicate();
      if (!newBuilder.remove(key)) {
        throw new IllegalStateException("Key '" + key + "' should be in the map");
      }
      return EnsoHashMap.createWithBuilder(newBuilder, newBuilder.getSize());
    }
  }

  @Specialization(
      guards = "interop.hasHashEntries(map)"
  )
  EnsoHashMap removeFromInteropMap(Object map, Object keyToRemove,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached HashCodeNode hashCodeNode,
      @Cached EqualsNode equalsNode) {
    // We cannot simply call interop.isHashEntryExisting, because it would, most likely
    // use the default `hashCode` and `equals` Java methods. But we need to use our
    // EqualsNode, so we do the check for non-existing key inside the while loop.
    boolean keyToRemoveFound = false;
    var mapBuilder = EnsoHashMapBuilder.create(hashCodeNode, equalsNode);
    try {
      Object entriesIterator = interop.getHashEntriesIterator(map);
      while (interop.hasIteratorNextElement(entriesIterator)) {
        Object keyValueArr = interop.getIteratorNextElement(entriesIterator);
        Object key = interop.readArrayElement(keyValueArr, 0);
        if ((boolean) equalsNode.execute(keyToRemove, key)) {
          if (keyToRemoveFound) {
            throw new IllegalStateException("Key " + key + " found twice");
          } else {
            keyToRemoveFound = true;
          }
        } else {
          Object value = interop.readArrayElement(keyValueArr, 1);
          mapBuilder.add(key, value);
        }
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      throw new IllegalStateException(
          "Polyglot hash map " + map + " has wrongly specified Interop API (hash entries iterator)",
          e
      );
    }
    if (keyToRemoveFound) {
      return EnsoHashMap.createWithBuilder(mapBuilder, mapBuilder.getSize());
    } else {
      CompilerDirectives.transferToInterpreter();
      throw DataflowError.withoutTrace("No such key " + keyToRemove, interop);
    }
  }
}
