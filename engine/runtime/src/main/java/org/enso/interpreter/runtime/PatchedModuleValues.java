package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.instrumentation.Instrumenter;
import com.oracle.truffle.api.instrumentation.SourceFilter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;
import org.enso.compiler.context.SimpleUpdate;
import org.enso.interpreter.instrument.IdExecutionService;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.tag.Patchable;
import org.enso.polyglot.LanguageInfo;

final class PatchedModuleValues implements ExecutionEventListener {
  private final TreeMap<Integer,int[]> deltas = new TreeMap<>();
  private final Map<Node, Object> values = new HashMap<>();
  private final Module module;
  private EventBinding<PatchedModuleValues> binding;

  PatchedModuleValues(Module module) {
    this.module = module;
  }

  void dispose() {
    if (binding != null) {
      binding.dispose();
      binding = null;
    }
  }

  private void registerUpdates(
    Instrumenter instr, Map<Node, Object> collect, int offset, int delta
  ) {
    if (binding == null) {
      SourceFilter sourceAssociatedWithMyModule = SourceFilter.newBuilder().
              languageIs(LanguageInfo.ID).
              sourceIs(module::isModuleSource).
              build();
      SourceSectionFilter filter = SourceSectionFilter.newBuilder().sourceFilter(sourceAssociatedWithMyModule).tagIs(Patchable.Tag.class).build();
      this.binding = instr.attachExecutionEventListener(filter, this);
    }
    values.putAll(collect);

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

  boolean simpleUpdate(Module module, SimpleUpdate update) {
    var scope = module.getScope();
    var methods = scope.getMethods();
    var conversions = scope.getConversions();
    var collect = new HashMap<Node, Object>();
    for (var n : values.keySet()) {
      updateNode(update, n, collect);
    }
    if (collect.isEmpty()) {
      // only search for new literals when none have been found
      updateFunctionsMap(update, methods.values(), collect);
      updateFunctionsMap(update, conversions.values(), collect);
      if (collect.isEmpty()) {
        return false;
      }
    }

    var ctx = Context.get(collect.keySet().iterator().next());
    var instr = ctx.getEnvironment().lookup(Instrumenter.class);
    final Source src;
    try {
      src = module.getSource();
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    }
    var edit = update.edit();
    int offset =
        src.getLineStartOffset(edit.range().start().line() + 1) + edit.range().start().character();
    int removed = edit.range().end().character() - edit.range().start().character();
    int delta = edit.text().length() - removed;

    registerUpdates(instr, collect, offset, delta);
    return true;
  }

  private static void updateFunctionsMap(SimpleUpdate edit, Collection<? extends Map<?, Function>> values, Map<Node, Object> nodeValues) {
    for (Map<?, Function> map : values) {
      for (Function f : map.values()) {
        updateNode(edit, f.getCallTarget().getRootNode(), nodeValues);
      }
    }
  }

  private static void updateNode(SimpleUpdate update, Node root, Map<Node, Object> nodeValues) {
    LinkedList<Node> queue = new LinkedList<>();
    queue.add(root);
    var edit = update.edit();
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
            java.lang.Object newValue = node.parsePatch(update.newIr());
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
      var unwind = context.createUnwind(patch);
      IdExecutionService.UNWIND_HELPER.registerValue(unwind, patch);
      throw unwind;
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
  private Object findPatch(Node n) {
    return values.get(n);
  }

  int findDelta(int offset, boolean inclusive) {
    Map.Entry<Integer, int[]> previous = inclusive ? deltas.floorEntry(offset) : deltas.lowerEntry(offset);
    return previous == null ? 0 : previous.getValue()[0];
  }
}
