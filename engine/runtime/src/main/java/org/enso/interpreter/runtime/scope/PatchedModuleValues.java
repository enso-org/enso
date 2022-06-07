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
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.tag.Patchable;
import org.enso.polyglot.LanguageInfo;
import org.enso.text.editing.model;

final class PatchedModuleValues implements ExecutionEventListener {

  private final Map<Node, Object> values = new HashMap<>();
  private final EventBinding<PatchedModuleValues> binding;
  private final ModuleScope outer;

  PatchedModuleValues(Instrumenter instr, final ModuleScope outer) {
    this.outer = outer;
    SourceFilter sourceAssociatedWithMyModule = SourceFilter.newBuilder().languageIs(LanguageInfo.ID).sourceIs(t -> {
      try {
        return outer.getModule().getSource() == t;
      } catch (IOException ex) {
        return false;
      }
    }).build();
    SourceSectionFilter filter = SourceSectionFilter.newBuilder().sourceFilter(sourceAssociatedWithMyModule).tagIs(Patchable.Tag.class).build();
    this.binding = instr.attachExecutionEventListener(filter, this);
  }

  void dispose() {
    binding.dispose();
  }

  void register(Map<Node, Object> collect) {
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
    root.accept(n -> {
      if (n instanceof Patchable node) {
        SourceSection at = n.getSourceSection();
        if (at != null && at.getStartLine() - 1 == edit.range().start().line() && at.getStartColumn() - 1 == edit.range().start().character() && at.getEndLine() - 1 == edit.range().end().line() && at.getEndColumn() == edit.range().end().character()) {
          java.lang.Object newValue = node.parsePatch(edit.text());
          if (newValue != null) {
            nodeValues.put(n, newValue);
          }
        }
      }
      return true;
    });
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
}
