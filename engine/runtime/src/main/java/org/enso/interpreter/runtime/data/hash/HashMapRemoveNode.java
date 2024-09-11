package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Dictionary",
    name = "remove_builtin",
    description = """
        Removes an entry from this map specified with the key.
        """)
@GenerateUncached
public abstract class HashMapRemoveNode extends Node {
  public static HashMapRemoveNode build() {
    return HashMapRemoveNodeGen.create();
  }

  public abstract EnsoHashMap execute(VirtualFrame frame, Object self, Object key);

  @Specialization
  EnsoHashMap removeFromEnsoMap(
      VirtualFrame frame,
      EnsoHashMap ensoMap,
      Object key,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode) {
    var mapBuilder = ensoMap.getMapBuilder(frame, false, hashCodeNode, equalsNode);
    if (mapBuilder.remove(frame, key, hashCodeNode, equalsNode)) {
      return mapBuilder.build();
    } else {
      throw DataflowError.withDefaultTrace("No such key", null);
    }
  }

  @Fallback
  EnsoHashMap removeFromInteropMap(
      VirtualFrame frame,
      Object map,
      Object keyToRemove,
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
        if (polyglotEquals(key, keyToRemove, frame, equalsNode, interop)) {
          if (keyToRemoveFound) {
            CompilerDirectives.transferToInterpreter();
            var ctx = EnsoContext.get(this);
            throw ctx.raiseAssertionPanic(this, "Key " + key + " found twice", null);
          } else {
            keyToRemoveFound = true;
          }
        } else {
          Object value = interop.readArrayElement(keyValueArr, 1);
          mapBuilder =
              mapBuilder.asModifiable(frame, mapBuilder.generation(), hashCodeNode, equalsNode);
          mapBuilder.put(frame, key, value, hashCodeNode, equalsNode);
        }
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      var msg =
          "Polyglot hash map " + map + " has wrongly specified Interop API (hash entries iterator)";
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(this, msg, e);
    }
    if (keyToRemoveFound) {
      return EnsoHashMap.createWithBuilder(mapBuilder);
    } else {
      CompilerDirectives.transferToInterpreter();
      throw DataflowError.withDefaultTrace("No such key " + keyToRemove, interop);
    }
  }

  /** A special case of equals - we want to be able to remove NaN from the map. */
  private boolean polyglotEquals(
      Object obj1, Object obj2, VirtualFrame frame, EqualsNode equalsNode, InteropLibrary interop) {
    if (isNan(obj1, interop) && isNan(obj2, interop)) {
      return true;
    } else {
      return equalsNode.execute(frame, obj1, obj2).equals();
    }
  }

  private boolean isNan(Object obj, InteropLibrary interop) {
    try {
      return interop.fitsInDouble(obj) && Double.isNaN(interop.asDouble(obj));
    } catch (UnsupportedMessageException e) {
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(this, null, e);
    }
  }
}
