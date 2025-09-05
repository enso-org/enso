package org.enso.compiler.pass.analyse;

import java.util.stream.Stream;
import org.enso.compiler.context.LocalScope;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.analyse.alias.graph.GraphOccurrence;

public sealed interface FrameAnalysisMeta extends ProcessingPass.Metadata
    permits FramePointer, FrameVariableNames {

  public static <I extends IR> I updateMetadata(I ir, FrameAnalysisMeta newMeta) {
    var thiz = FramePointerAnalysis$.MODULE$;
    var opt = ir.passData().get(thiz);
    if (opt.isEmpty()) {
      ir.passData().update(thiz, newMeta);
      return ir;
    } else {
      var meta = opt.get();
      var ex =
          new IllegalStateException(
              "Unexpected FrameAnalysisMeta associated with IR "
                  + ir
                  + "\nOld: "
                  + meta
                  + " new "
                  + newMeta);
      var topTenFrames = Stream.of(ex.getStackTrace()).limit(10).toArray(StackTraceElement[]::new);
      ex.setStackTrace(topTenFrames);
      throw ex;
    }
  }

  static void updateSymbolNames(IR e, Graph.Scope s) {
    var symbols = s.allDefinitions().stream().map(d -> d.symbol()).toList();
    updateMetadata(e, FrameVariableNames.create(symbols));
  }

  static void updateOccurrance(IR ir, Graph graph, Graph.Scope scope, GraphOccurrence occ) {
    switch (occ) {
      case GraphOccurrence.Use use -> {
        var linkOpt = graph.defLinkFor(use.id());
        // Use is allowed to read a variable from some parent scope
        if (linkOpt.isDefined()) {
          var defLink = linkOpt.get();
          var defId = defLink.target();
          var defOcc = (GraphOccurrence.Def) graph.getOccurrence(defId).get();
          var defScope = graph.scopeFor(defId).get();
          var parentLevel = getScopeDistance(defScope, scope);
          var frameSlotIdx = getFrameSlotIdxInScope(graph, defScope, defOcc);
          updateMetadata(ir, new FramePointer(parentLevel, frameSlotIdx));
        }
      }
      case GraphOccurrence.Def defn -> {
        // The definition cannot write to parent's frame slots.
        var parentLevel = 0;
        var frameSlotIdx = getFrameSlotIdxInScope(graph, scope, defn);
        FrameAnalysisMeta.updateMetadata(ir, new FramePointer(parentLevel, frameSlotIdx));
      }
    }
  }

  /**
   * Returns the *scope distance* of the given `childScope` to the given `parentScope`. Scope
   * distance is the number of parents from the `childScope`.
   *
   * @param parentScope Some of the parent scopes of `childScope`.
   * @param childScope Nested child scope of `parentScope`.
   * @return
   */
  private static int getScopeDistance(Graph.Scope parentScope, Graph.Scope childScope) {
    var currScope = childScope;
    var scopeDistance = 0;
    while (currScope != null && currScope != parentScope) {
      var opt = currScope.parent();
      scopeDistance++;
      currScope = opt.isDefined() ? opt.get() : null;
    }
    return scopeDistance;
  }

  /**
   * Returns the index of the given `defOcc` definition in the given `scope`
   *
   * @param scope This scope must contain the given `defOcc`
   * @param defOcc This occurrence must be in the given `scope`
   */
  private static int getFrameSlotIdxInScope(
      Graph graph, Graph.Scope scope, GraphOccurrence.Def defOcc) {
    assert graph.scopeFor(defOcc.id()).contains(scope)
        : "Def occurrence must be in the given scope";

    var it = scope.allDefinitions().iterator();
    var i = 0;
    while (it.hasNext()) {
      var def = it.next();
      if (def.id() == defOcc.id()) {
        return i + LocalScope.internalSlotsSize();
      }
      i++;
    }
    throw new IllegalStateException("Def occurrence must be in the given scope");
  }
}
