package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.common.LanguageInfo;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;

@GenerateUncached
@NodeInfo(
    shortName = "MapInsertAll",
    description = "Inserts all elements from the given container into a hash map",
    language = LanguageInfo.ID)
public abstract class HashMapInsertAllNode extends Node {

  public static HashMapInsertAllNode build() {
    return HashMapInsertAllNodeGen.create();
  }

  public static HashMapInsertAllNode getUncached() {
    return HashMapInsertAllNodeGen.getUncached();
  }

  /**
   * Insert all the elements from the given container into the given map.
   *
   * @param self A map to insert into.
   * @param container Either a map, or a list of pairs to insert into the map.
   * @param maxItems Maximum number of items to insert into the map from the container.
   */
  public abstract EnsoHashMap executeInsertAll(
      VirtualFrame frame, EnsoHashMap self, EnsoHashMap container, long maxItems);

  @Specialization
  EnsoHashMap doEnsoHashMaps(
      VirtualFrame frame,
      EnsoHashMap self,
      EnsoHashMap other,
      long maxItems,
      @Cached HashCodeNode hashCodeNode,
      @Cached EqualsNode equalsNode) {
    if (maxItems <= 0) {
      return self;
    }
    var selfSize = self.getHashSize();
    var otherSize = other.getHashSize();
    if (otherSize == 0) {
      return self;
    }
    var exactCapacity = selfSize + otherSize;
    var enoughCapacity = exactCapacity + exactCapacity / 2;
    var mapBuilder = EnsoHashMapBuilder.createWithCapacity(enoughCapacity);

    var selfEntriesIt = self.getEntriesIterator(frame, hashCodeNode, equalsNode);
    while (selfEntriesIt.hasNext()) {
      var selfEntry = selfEntriesIt.next();
      mapBuilder.put(frame, selfEntry.key(), selfEntry.value(), hashCodeNode, equalsNode);
    }
    var otherEntriesIt = other.getEntriesIterator(frame, hashCodeNode, equalsNode);
    var itemsInserted = 0;
    while (otherEntriesIt.hasNext()) {
      if (itemsInserted >= maxItems) {
        break;
      }
      var entry = otherEntriesIt.next();
      mapBuilder.put(frame, entry.key(), entry.value(), hashCodeNode, equalsNode);
      itemsInserted++;
    }
    return mapBuilder.build();
  }
}
