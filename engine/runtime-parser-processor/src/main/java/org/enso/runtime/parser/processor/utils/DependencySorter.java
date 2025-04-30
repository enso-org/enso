package org.enso.runtime.parser.processor.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

public final class DependencySorter {
  private DependencySorter() {}

  public static void ensureNoCycles(Map<String, Set<String>> deps)
      throws CyclicDependencyException {
    for (var depEntry : deps.entrySet()) {
      var from = depEntry.getKey();
      var hasCyclicDep = deps.values().stream().anyMatch(set -> set.contains(from));
      if (hasCyclicDep) {
        throw new CyclicDependencyException(from + " -> " + from);
      }
    }
  }

  /**
   * Based on Kahn's algorithm for topological sorting.
   *
   * @param deps
   * @return
   */
  public static List<String> topologicalSort(Map<String, Set<String>> deps) {
    Map<String, Integer> inDegree = new HashMap<>();
    Map<String, Set<String>> adjList = new HashMap<>();

    // Initialize in-degree and adjacency list
    for (var entry : deps.entrySet()) {
      var node = entry.getKey();
      var neighbors = entry.getValue();
      inDegree.putIfAbsent(node, 0);
      adjList.putIfAbsent(node, new HashSet<>());
      for (var neighbor : neighbors) {
        adjList.putIfAbsent(neighbor, new HashSet<>());
        inDegree.put(neighbor, inDegree.getOrDefault(neighbor, 0) + 1);
        adjList.get(node).add(neighbor);
      }
    }

    // Collect nodes with no incoming edges
    Queue<String> queue = new LinkedList<>();
    for (var entry : inDegree.entrySet()) {
      if (entry.getValue() == 0) {
        queue.add(entry.getKey());
      }
    }

    List<String> sorted = new ArrayList<>();
    while (!queue.isEmpty()) {
      var node = queue.poll();
      sorted.add(node);

      for (var neighbor : adjList.get(node)) {
        inDegree.put(neighbor, inDegree.get(neighbor) - 1);
        if (inDegree.get(neighbor) == 0) {
          queue.add(neighbor);
        }
      }
    }

    // Check for cycles
    if (sorted.size() != inDegree.size()) {
      throw new IllegalStateException("Graph has at least one cycle");
    }

    Collections.reverse(sorted);
    return sorted;
  }

  public static final class CyclicDependencyException extends Exception {
    public CyclicDependencyException(String message) {
      super(message);
    }
  }
}
