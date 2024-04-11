package org.enso.polyfill.web;

import java.util.Arrays;
import org.enso.polyfill.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

public final class PerformancePolyfill implements ProxyExecutable, Polyfill {

    private static final String NOW = "now";

    private static final String PERFORMANCE_POLYFILL_JS = "performance-polyfill.js";

    public PerformancePolyfill() {
    }

    @Override
    public void initialize(Context ctx) {
        Source performancePolyfillJs = Source
                .newBuilder("js", PerformancePolyfill.class.getResource(PERFORMANCE_POLYFILL_JS))
                .buildLiteral();

        ctx.eval(performancePolyfillJs).execute(this);
    }

    @Override
    public Object execute(Value... arguments) {
        var command = arguments[0].asString();
        System.err.println(command + " " + Arrays.toString(arguments));

        return switch (command) {
            case NOW ->
                System.currentTimeMillis();

            default ->
                throw new IllegalStateException(command);
        };
    }
}
