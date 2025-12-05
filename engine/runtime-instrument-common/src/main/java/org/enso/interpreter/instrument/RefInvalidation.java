package org.enso.interpreter.instrument;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.UUID;
import java.util.stream.Collectors;
import org.enso.interpreter.runtime.execution.Ref;
import org.enso.interpreter.runtime.execution.RuntimeAnalysis;
import org.enso.polyglot.ExternalUUID;
import org.enso.polyglot.RuntimeID;

public class RefInvalidation {

  /**
   * Infers a transitive set of IDs rendered invalid as a result of changes to initial set of UUIDs.
   *
   * @param initial intiial set of UUIDS affected by the change
   * @param frames stack of frames containing caches, used for tracking changes between function
   *     calls
   * @return a transitive closure of IDs affected
   */
  public static Set<RuntimeID> invalidateAffectedIDs(
      Iterable<UUID> initial,
      Stack<InstrumentFrame> frames,
      VisualizationHolder visHolder,
      RuntimeAnalysis runtimeAnalysis) {
    @SuppressWarnings("unchecked")
    var frames1 = (Stack<InstrumentFrame>) frames.clone();
    if (frames1.isEmpty()) {
      return Set.of();
    } else {
      var cache = frames1.pop();
      var directlyInvalidated = new LinkedList<Ref>();
      for (UUID uuid : initial) {
        var ref = runtimeAnalysis.get(ExternalUUID.create(uuid));
        if (ref != null) {
          directlyInvalidated.add(ref);
        }
      }
      var transitivelyInvalidated =
          invalidateTransitiveDependencies(
              directlyInvalidated,
              Set.of(),
              frames1.stream().toList(),
              cache,
              null,
              visHolder,
              runtimeAnalysis,
              true);
      return transitivelyInvalidated.stream()
          .filter(AffectedID::currentFrame)
          .map(AffectedID::id)
          .collect(Collectors.toSet());
    }
  }

  /**
   * Follows dependencies that cross function/runtime caches boundaries
   *
   * @param toInvalidate sequence of dependencies remaining to process
   * @param acc a set of dependencies already invalidated
   * @param frames a stack of runtime caches that could be analyzed for dependencies
   * @param currentCallID ID of the currently entered function call, null if in the top frame
   */
  private static Set<AffectedID> invalidateTransitiveDependencies(
      List<Ref> toInvalidate,
      Set<AffectedID> acc,
      List<InstrumentFrame> frames,
      InstrumentFrame currentFrame,
      RuntimeID currentCallID,
      VisualizationHolder visHolder,
      RuntimeAnalysis runtimeAnalysis,
      boolean topFrame) {
    var newAcc = new HashSet<>(acc);
    while (!toInvalidate.isEmpty()) {
      var head = toInvalidate.remove(0);
      newAcc.add(new AffectedID(head.getRuntimeID(), topFrame));

      // Invalidate reference and get all dependents
      var toProcessInCurrentCache = head.reset().filter(o -> !newAcc.contains(o.getRuntimeID()));

      // Invalidate associated cache entry
      currentFrame.cache().remove(head.getRuntimeID()); // Ignore cache result

      // Invalidate associated visualizations
      visHolder
          .find(head.getRuntimeID().uuid())
          .foreach(
              (visualization) -> {
                currentFrame.syncState().setVisualizationUnsync(visualization.id());
                return null;
              });
      Set<AffectedID> unrolledDependencies = Set.of();
      // FIXME: For now let's ignore nesting runtime caches.
      if (!frames.isEmpty()) { // && head.getRuntimeID().equals(currentCallID)) {
        var cacheOneLevelUp = frames.get(0);
        var refOneLevelUp = cacheOneLevelUp.cache().get(head.getRuntimeID());
        if (refOneLevelUp != null) {
          var frames1 = new LinkedList<>(frames);
          // var top = caches1.removeFirst();
          RuntimeID nextCallID = null; // top.getLocalCallUUID();
          var toInvalidate1 = new LinkedList<Ref>();
          toInvalidate1.add(runtimeAnalysis.get(head.getRuntimeID()));
          unrolledDependencies =
              invalidateTransitiveDependencies(
                  toInvalidate1,
                  Set.of(),
                  frames1,
                  cacheOneLevelUp,
                  nextCallID,
                  visHolder,
                  runtimeAnalysis,
                  false);
        }
      }
      var toProcessList = toProcessInCurrentCache.toList();
      newAcc.addAll(unrolledDependencies);
      toInvalidate.addAll(toProcessList);
    }
    return newAcc;
  }

  public record AffectedID(RuntimeID id, boolean currentFrame) {
    @Override
    public boolean equals(Object o) {
      return o instanceof AffectedID obj && this.id.equals(obj.id);
    }

    @Override
    public int hashCode() {
      return id.hashCode();
    }
  }
}
