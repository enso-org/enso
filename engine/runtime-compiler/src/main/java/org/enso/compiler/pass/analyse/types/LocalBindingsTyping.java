package org.enso.compiler.pass.analyse.types;

import java.util.HashMap;
import java.util.Map;
import org.enso.compiler.pass.analyse.AliasAnalysis;
import org.enso.compiler.pass.analyse.AliasAnalysisGraph;

/**
 * A mapping that maps binding definitions from an alias analysis graph to their inferred types.
 *
 * <p>An entry is inserted only if a given binding had an inferred type.
 */
class LocalBindingsTyping {
  private final Map<AliasAnalysisGraph, Map<Integer, TypeRepresentation>> map = new HashMap<>();

  public static LocalBindingsTyping create() {
    return new LocalBindingsTyping();
  }

  private Map<Integer, TypeRepresentation> accessGraph(AliasAnalysisGraph graph) {
    return map.computeIfAbsent(graph, (g) -> new HashMap<>());
  }

  TypeRepresentation getBindingType(AliasAnalysisGraph graph, int id) {
    return accessGraph(graph).get(id);
  }

  void registerBindingType(AliasAnalysisGraph graph, int id, TypeRepresentation type) {
    var previous = accessGraph(graph).put(id, type);
    if (previous != null) {
      throw new IllegalStateException("Duplicate binding " + id + " in graph " + graph);
    }
  }
}
