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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RefInvalidation {

  private static final Logger LOGGER = LoggerFactory.getLogger(RefInvalidation.class);

  /**
   * Infers a transitive set of IDs rendered invalid as a result of changes to initial set of UUIDs.
   *
   * @param initial intiial set of UUIDS affected by the change
   * @param frames stack of frames containing caches, used for tracking changes between function
   *     calls
   * @param visHolder a reference to all registered visualizations
   * @param runtimeAnalysis dependency tracking info
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

      var frame = frames1.pop();
      if (runtimeAnalysis == null) {
        LOGGER.debug("No runtime data. Unable to do any invalidation for " + initial);
        return Set.of();
      }
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
              frame,
              visHolder,
              runtimeAnalysis,
              true);
      var result =
          transitivelyInvalidated.stream()
              .filter(AffectedID::currentFrame)
              .map(AffectedID::id)
              .collect(Collectors.toSet());
      return result;
    }
  }

  /**
   * Follows dependencies that cross function/runtime caches boundaries
   *
   * @param toInvalidate sequence of dependencies remaining to process
   * @param acc a set of dependencies already invalidated
   * @param frames a stack of runtime caches that could be analyzed for dependencies
   */
  private static Set<AffectedID> invalidateTransitiveDependencies(
      List<Ref> toInvalidate,
      Set<AffectedID> acc,
      List<InstrumentFrame> frames,
      InstrumentFrame currentFrame,
      VisualizationHolder visHolder,
      RuntimeAnalysis runtimeAnalysis,
      boolean topFrame) {
    var newAcc = new HashSet<>(acc);
    while (!toInvalidate.isEmpty()) {
      var head = toInvalidate.remove(0);
      newAcc.add(new AffectedID(head.getRuntimeID(), topFrame));

      // Invalidate reference and get all dependents
      var toProcessInCurrentCache =
          head.reset().filter(o -> !newAcc.contains(new AffectedID(o.getRuntimeID(), topFrame)));

      // Invalidate associated cache entry
      currentFrame.cache().remove(head.getRuntimeID());

      // Invalidate associated visualizations
      visHolder
          .find(head.getRuntimeID().uuid())
          .foreach(
              (visualization) -> {
                currentFrame.syncState().setVisualizationUnsync(visualization.id());
                return null;
              });
      Set<AffectedID> unrolledDependencies = Set.of();
      if (!frames.isEmpty()) {
        var frameOneLevelUp = frames.get(0);
        var refOneLevelUp = frameOneLevelUp.cache().get(head.getRuntimeID());
        if (refOneLevelUp != null) {
          var frames1 = new LinkedList<>(frames);
          var toInvalidate1 = new LinkedList<Ref>();
          toInvalidate1.add(runtimeAnalysis.get(head.getRuntimeID()));
          unrolledDependencies =
              invalidateTransitiveDependencies(
                  toInvalidate1,
                  Set.of(),
                  frames1,
                  frameOneLevelUp,
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

  // FIXME: simplify usage
  record AffectedID(RuntimeID id, boolean currentFrame) {
    @Override
    public boolean equals(Object o) {
      return o instanceof AffectedID obj && this.id.equals(obj.id)
          || o instanceof RuntimeID runtimeID && this.id.equals(runtimeID);
    }

    @Override
    public int hashCode() {
      return id.hashCode();
    }
  }
}
