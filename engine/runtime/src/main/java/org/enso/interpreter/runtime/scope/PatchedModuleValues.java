package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.instrumentation.Instrumenter;
import com.oracle.truffle.api.instrumentation.SourceFilter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.tag.Patchable;
import org.enso.polyglot.LanguageInfo;
import org.enso.text.editing.model;

final class PatchedModuleValues implements ExecutionEventListener {
  private final TreeMap<Integer,int[]> deltas = new TreeMap<>();
  private final Map<Node, Object> values = new HashMap<>();
  private final EventBinding<PatchedModuleValues> binding;

  PatchedModuleValues(Instrumenter instr, Module module) {
    SourceFilter sourceAssociatedWithMyModule = SourceFilter.newBuilder().
      languageIs(LanguageInfo.ID).
      sourceIs(module::isModuleSource).
      build();
    SourceSectionFilter filter = SourceSectionFilter.newBuilder().sourceFilter(sourceAssociatedWithMyModule).tagIs(Patchable.Tag.class).build();
    this.binding = instr.attachExecutionEventListener(filter, this);
  }

  void dispose() {
    binding.dispose();
  }

  void registerValues(Map<Node, Object> collect) {
    values.putAll(collect);
  }

  static void updateFunctionsMap(model.TextEdit edit, Collection<? extends Map<?, Function>> values, Map<Node, Object> nodeValues) {
    for (Map<?, Function> map : values) {
      for (Function f : map.values()) {
        updateNode(edit, f.getCallTarget().getRootNode(), nodeValues);
      }
    }
  }

  private static void updateNode(model.TextEdit edit, Node root, Map<Node, Object> nodeValues) {
    LinkedList<Node> queue = new LinkedList<>();
    queue.add(root);
    while (!queue.isEmpty()) {
      var n = queue.removeFirst();
      SourceSection at = n.getSourceSection();
      if (at != null) {
        if (at.getEndLine() - 1 < edit.range().start().line()) {
          continue;
        }
        if (at.getStartLine() - 1 > edit.range().end().line()) {
          continue;
        }
        if (n instanceof Patchable node) {
          if (
            at.getStartLine() - 1 == edit.range().start().line() &&
            at.getStartColumn() - 1 == edit.range().start().character() &&
            at.getEndLine() - 1 == edit.range().end().line() &&
            at.getEndColumn() == edit.range().end().character()
          ) {
            java.lang.Object newValue = node.parsePatch(edit.text());
            if (newValue != null) {
              nodeValues.put(n, newValue);
            }
          }
        }
      }
      for (var ch : n.getChildren()) {
        queue.add(ch);
      }
    }
  }

  @Override
  public void onEnter(EventContext context, VirtualFrame frame) {
    Object patch = findPatch(context.getInstrumentedNode());
    if (patch != null) {
      throw context.createUnwind(patch);
    }
  }

  @Override
  public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
  }

  @Override
  public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
  }

  @Override
  public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
    return info;
  }

  @CompilerDirectives.TruffleBoundary
  public Object findPatch(Node n) {
    return values.get(n);
  }

  void registerDelta(int offset, int delta) {
    if (delta == 0) {
      return;
    }
    Map.Entry<Integer, int[]> previous = deltas.floorEntry(offset);
    if (previous == null) {
      deltas.put(offset, new int[] { delta });
    } else if (previous.getKey() == offset) {
      previous.getValue()[0] += delta;
    } else {
      deltas.put(offset, new int[] { previous.getValue()[0] + delta });
    }
    for (int[] after : deltas.tailMap(offset, false).values()) {
      after[0] += delta;
    }
  }

  int findDelta(int offset, boolean inclusive) {
    Map.Entry<Integer, int[]> previous = inclusive ? deltas.floorEntry(offset) : deltas.lowerEntry(offset);
    return previous == null ? 0 : previous.getValue()[0];
  }
}
