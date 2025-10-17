package org.enso.interpreter.instrument;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.UUID;
import java.util.stream.Collectors;
import org.enso.polyglot.RuntimeID;

public class ObservableInvalidation {

  /**
   * Infers a transitive set of IDs rendered invalid as a result of changes to initial set of UUIDs.
   *
   * @param initial intiial set of UUIDS affected by the change
   * @param frames stack of frames containing caches, used for tracking changes between function
   *     calls
   * @return a transitive closure of IDs affected
   */
  public static Set<RuntimeID> invalidateAffectedIDs(
      Iterable<UUID> initial, Stack<InstrumentFrame> frames) {
    @SuppressWarnings("unchecked")
    var frames1 = (Stack<InstrumentFrame>) frames.clone();
    if (frames1.isEmpty()) {
      return Set.of();
    } else {
      var cache = frames1.pop().cache();
      var rest = frames1.stream().map(InstrumentFrame::cache).toList();
      var directlyInvalidated = new LinkedList<Observable>();
      for (UUID uuid : initial) {
        var observable = cache.get(uuid);
        if (observable != null) {
          directlyInvalidated.add(observable);
        }
      }
      var transitivelyInvalidated =
          invalidateTransitiveDependencies(
              directlyInvalidated, Set.of(), rest, cache.getLocalCallUUID());
      return transitivelyInvalidated.stream().map(Observable::id).collect(Collectors.toSet());
    }
  }

  /**
   * Follows dependencies that cross function/runtime caches boundaries
   *
   * @param toInvalidate sequence of dependencies remaining to process
   * @param acc a set of dependencies already invalidated
   * @param caches a stack of runtime caches that could be analyzed for dependencies
   * @param currentCallID ID of the currently entered function call, null if in the top frame
   */
  private static Set<Observable> invalidateTransitiveDependencies(
      List<Observable> toInvalidate,
      Set<Observable> acc,
      List<RuntimeCache> caches,
      RuntimeID currentCallID) {
    if (toInvalidate.isEmpty()) {
      return acc;
    } else {
      var head = toInvalidate.remove(0);
      var toProcessInCurrentCache = head.invalidate().filter(o -> !acc.contains(o));
      Set<Observable> unrolledDependencies = Set.of();
      if (!caches.isEmpty() && head.id() == currentCallID) {
        var observableOneLevelUp = caches.get(0).get(head.id());
        if (observableOneLevelUp != null) {
          var caches1 = new LinkedList<>(caches);
          var top = caches1.removeFirst();
          var nextCallID = top.getLocalCallUUID();
          var toInvalidate1 = new LinkedList<Observable>();
          toInvalidate1.add(observableOneLevelUp);
          unrolledDependencies =
              invalidateTransitiveDependencies(toInvalidate1, Set.of(), caches1, nextCallID);
        }
      }
      var newAcc = new HashSet<Observable>();
      newAcc.add(head);
      newAcc.addAll(acc);
      newAcc.addAll(unrolledDependencies);
      toInvalidate.addAll(toProcessInCurrentCache.toList());
      return invalidateTransitiveDependencies(toInvalidate, newAcc, caches, currentCallID);
    }
  }

  public static void invalidateDownstreamDependencies(RuntimeID initial, List<RuntimeCache> caches) {
    if (!caches.isEmpty()) {
      var cache = caches.remove(0);
      var obs = cache.get(initial);
      assert obs != null;
      var toProcess = cache.downstreamOf(initial);
      var processed = new HashSet<RuntimeID>();
      processed.add(initial);
      while (!toProcess.isEmpty()) {
        var head = toProcess.remove(0);
        head.invalidate();
        processed.add(head.id());
        if (cache.get(head.id()) != null) {
          for (Observable downstream : cache.downstreamOf(head.id())) {
            if (!processed.contains(downstream.id()) && !toProcess.contains(downstream)) {
              toProcess.add(downstream);
            }
          }
        } else {
          invalidateDownstreamDependencies(head.id(), caches);
        }
      }
    }
  }
}
