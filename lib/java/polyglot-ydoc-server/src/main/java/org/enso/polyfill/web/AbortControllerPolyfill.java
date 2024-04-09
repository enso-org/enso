package org.enso.polyfill.web;

import java.util.Arrays;
import java.util.UUID;
import org.enso.polyfill.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

public final class AbortControllerPolyfill implements ProxyExecutable, Polyfill {

    private static final String RANDOM_UUID = "random-uuid";

    private static final String ABORT_CONTROLLER_POLYFILL_JS = "abort-controller-polyfill.js";

    public AbortControllerPolyfill() {
    }

    @Override
    public void initialize(Context ctx) {
        Source abortControllerPolyfillJs = Source
                .newBuilder("js", AbortControllerPolyfill.class.getResource(ABORT_CONTROLLER_POLYFILL_JS))
                .buildLiteral();

        ctx.eval(abortControllerPolyfillJs).execute(this);
    }

    @Override
    public Object execute(Value... arguments) {
        var command = arguments[0].asString();
        System.err.println(command + " " + Arrays.toString(arguments));

        return switch (command) {
            case RANDOM_UUID ->
                UUID.randomUUID().toString();

            default ->
                throw new IllegalStateException(command);
        };
    }
}
