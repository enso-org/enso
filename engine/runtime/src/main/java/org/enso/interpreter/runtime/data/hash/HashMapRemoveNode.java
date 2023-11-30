package org.enso.interpreter.runtime.data.hash;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;

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
  EnsoHashMap removeFromEnsoMap(
    EnsoHashMap ensoMap, Object key,
    @Shared("hash") @Cached HashCodeNode hashCodeNode,
    @Shared("equals") @Cached EqualsNode equalsNode
  ) {
    var mapBuilder = ensoMap.getMapBuilder(false, hashCodeNode, equalsNode);
    if (mapBuilder.remove(key, hashCodeNode, equalsNode)) {
      return mapBuilder.build();
    } else {
      throw DataflowError.withoutTrace("No such key", null);
    }
  }

  @Specialization(
      guards = "interop.hasHashEntries(map)"
  )
  EnsoHashMap removeFromInteropMap(Object map, Object keyToRemove,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode) {
    // We cannot simply call interop.isHashEntryExisting, because it would, most likely
    // use the default `hashCode` and `equals` Java methods. But we need to use our
    // EqualsNode, so we do the check for non-existing key inside the while loop.
    boolean keyToRemoveFound = false;
    var mapBuilder = EnsoHashMapBuilder.create();
    try {
      Object entriesIterator = interop.getHashEntriesIterator(map);
      while (interop.hasIteratorNextElement(entriesIterator)) {
        Object keyValueArr = interop.getIteratorNextElement(entriesIterator);
        Object key = interop.readArrayElement(keyValueArr, 0);
        if ((boolean) equalsNode.execute(keyToRemove, key)) {
          if (keyToRemoveFound) {
            CompilerDirectives.transferToInterpreter();
            throw new PanicException(Text.create("Key " + key + " found twice"), this);
          } else {
            keyToRemoveFound = true;
          }
        } else {
          Object value = interop.readArrayElement(keyValueArr, 1);
          mapBuilder = mapBuilder.asModifiable(mapBuilder.generation(), hashCodeNode, equalsNode);
          mapBuilder.put(key, value, hashCodeNode, equalsNode);
        }
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      var msg = "Polyglot hash map " + map + " has wrongly specified Interop API (hash entries iterator)";
      throw new PanicException(Text.create(msg), this);
    }
    if (keyToRemoveFound) {
      return EnsoHashMap.createWithBuilder(mapBuilder);
    } else {
      CompilerDirectives.transferToInterpreter();
      throw DataflowError.withoutTrace("No such key " + keyToRemove, interop);
    }
  }
}
