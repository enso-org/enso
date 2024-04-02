package org.enso.polyfill.encoding;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import org.enso.polyfill.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

public final class EncodingPolyfill implements ProxyExecutable, Polyfill {

    private static final String TEXT_DECODER_DECODE = "text-decoder-decode";

    private static final String ENCODING_POLYFILL_JS = "encoding-polyfill.js";

    public EncodingPolyfill() {
    }

    @Override
    public void initialize(Context ctx) {
        Source encodingPolyfillJs = Source
                .newBuilder("js", EncodingPolyfill.class.getResource(ENCODING_POLYFILL_JS))
                .buildLiteral();

        ctx.eval(encodingPolyfillJs).execute(this);
    }

    @Override
    public Object execute(Value... arguments) {
        var command = arguments[0].asString();
        System.err.println(command + " " + Arrays.toString(arguments));

        return switch (command) {
            case TEXT_DECODER_DECODE -> {
                var encoding = arguments[1].asString();
                var data = arguments[2].as(int[].class);

                var charset = encoding == null ? StandardCharsets.UTF_8 : Charset.forName(encoding);
                // Convert unsigned Uint8Array to byte[]
                var bytes = new byte[data.length];
                for (int i = 0; i < data.length; i++) {
                    bytes[i] = (byte) data[i];
                }

                yield charset.decode(ByteBuffer.wrap(bytes)).toString();
            }

            default ->
                throw new IllegalStateException(command);
        };
    }
}
