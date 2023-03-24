package org.enso.interpreter.test;

import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.pkg.QualifiedName;

import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/** Testing instrument to control newly created nodes. */
@TruffleInstrument.Registration(
        id = FunctionCallTestInstrument.INSTRUMENT_ID,
        services = FunctionCallTestInstrument.class)
public class FunctionCallTestInstrument extends TruffleInstrument {
    public static final String INSTRUMENT_ID = "function-call-test";

    private Map<UUID, FunctionCallInfo> calls = new ConcurrentHashMap<>();
    private Env env;

    @Override
    protected void onCreate(Env env) {
        env.registerService(this);
        this.env = env;
    }

    public void enable(SourceSectionFilter filter) {
        this.env
                .getInstrumenter()
                .attachExecutionEventFactory(filter, new FunctionCallFactory());
    }

    public Map<UUID, FunctionCallInfo> registeredCalls() {
        return calls;
    }

    private final class FunctionCallFactory implements ExecutionEventNodeFactory {
        @Override
        public ExecutionEventNode create(EventContext context) {
            final Node node = context.getInstrumentedNode();
            if (!"PatchableLiteralNode".equals(node.getClass().getSimpleName())) {
                return new NodeWrapper(context, calls);
            }
            return null;
        }
    }

    private class NodeWrapper extends ExecutionEventNode {

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
            calls.put(node.getId(), new FunctionCallInfo(result));
        }

    }

    public class FunctionCallInfo {

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
