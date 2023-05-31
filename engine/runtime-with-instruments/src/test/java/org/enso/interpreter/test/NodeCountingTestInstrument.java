package org.enso.interpreter.test;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.pkg.QualifiedName;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Function;
import static org.junit.Assert.fail;

/** Testing instrument to control newly created nodes. */
@TruffleInstrument.Registration(
    id = NodeCountingTestInstrument.INSTRUMENT_ID,
    services = NodeCountingTestInstrument.class)
public class NodeCountingTestInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "node-count-test";
  private final Map<Node, Node> all = new ConcurrentHashMap<>();
  private Map<Class, List<Node>> counter = new ConcurrentHashMap<>();

  private final Map<UUID, FunctionCallInfo> calls = new ConcurrentHashMap<>();
  private Env env;

  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    this.env = env;
  }

  public void enable() {
    this.env
        .getInstrumenter()
        .attachExecutionEventFactory(SourceSectionFilter.ANY, new CountingAndFunctionCallFactory());
  }

  public void enable(SourceSectionFilter filter) {
    this.env
            .getInstrumenter()
            .attachExecutionEventFactory(filter, new CountingAndFunctionCallFactory());
  }

  public Map<UUID, FunctionCallInfo> registeredCalls() {
    return calls;
  }

  public Map<Class, List<Node>> assertNewNodes(String msg, int min, int max) {
    Map<Class, List<Node>> prev = counter;
    long value = prev.values().stream().mapToInt(List::size).sum();

    Function<String, String> dump =
        (txt) -> {
          var sb = new StringBuilder(txt);
          prev.values().stream()
              .forEach(
                  (t) -> {
                    t.forEach(
                        (n) -> {
                          dumpNode("", n, sb);
                        });
                  });
          return sb.toString();
        };

    if (value < min) {
      fail(dump.apply(msg + ". Minimal size should be " + min + ", but was: " + value + " in"));
    }
    if (value > max) {
      fail(dump.apply(msg + ". Maximal size should be " + max + ", but was: " + value + " in"));
    }
    counter = new ConcurrentHashMap<>();
    return prev;
  }

  private void dumpNode(String indent, Node n, StringBuilder sb) {
    sb.append("\n").append(indent);
    sb.append(n.getClass().getName());
    final SourceSection ss = n.getSourceSection();
    if (ss != null) {
      sb.append(" @ ").append(ss.getSource().getName()).append(":").append(ss.getStartLine());
    }
  }

  private final class CountingAndFunctionCallFactory implements ExecutionEventNodeFactory {
    @Override
    public ExecutionEventNode create(EventContext context) {
      final Node node = context.getInstrumentedNode();
      if (!"PatchableLiteralNode".equals(node.getClass().getSimpleName())) {
        if (all.put(node, node) == null) {
          counter.computeIfAbsent(node.getClass(), (__) -> new CopyOnWriteArrayList<>()).add(node);
        }
        return new NodeWrapper(context, calls);
      }
      return null;
    }
  }

  private static class NodeWrapper extends ExecutionEventNode {

    private final EventContext context;

    private final Map<UUID, FunctionCallInfo> calls;

    public NodeWrapper(EventContext context, Map<UUID, FunctionCallInfo> calls) {
      this.context = context;
      this.calls = calls;
    }

    public void onReturnValue(VirtualFrame frame, Object result) {
      Node node = context.getInstrumentedNode();
      if (node instanceof FunctionCallInstrumentationNode instrumentableNode
              && result instanceof FunctionCallInstrumentationNode.FunctionCall functionCall) {
        onFunctionReturn(instrumentableNode, functionCall);
      }
    }

    private void onFunctionReturn(FunctionCallInstrumentationNode node, FunctionCallInstrumentationNode.FunctionCall result) {
      if (node.getId() != null) {
        calls.put(node.getId(), new FunctionCallInfo(result));
      }
    }

  }

  public static class FunctionCallInfo {

    private final QualifiedName moduleName;
    private final QualifiedName typeName;
    private final String functionName;

    public FunctionCallInfo(FunctionCallInstrumentationNode.FunctionCall call) {
      RootNode rootNode = call.getFunction().getCallTarget().getRootNode();
      if (rootNode instanceof MethodRootNode methodNode) {
        moduleName = methodNode.getModuleScope().getModule().getName();
        typeName = methodNode.getType().getQualifiedName();
        functionName = methodNode.getMethodName();
      } else {
        moduleName = null;
        typeName = null;
        functionName = rootNode.getName();
      }
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      FunctionCallInfo that = (FunctionCallInfo) o;
      return Objects.equals(moduleName, that.moduleName)
              && Objects.equals(typeName, that.typeName)
              && Objects.equals(functionName, that.functionName);
    }

    @Override
    public int hashCode() {
      return Objects.hash(moduleName, typeName, functionName);
    }

    @Override
    public String toString() {
      return moduleName + "::" + typeName + "::" + functionName;
    }

    public QualifiedName getModuleName() {
      return moduleName;
    }

    public QualifiedName getTypeName() {
      return typeName;
    }

    public String getFunctionName() {
      return functionName;
    }
  }
}
