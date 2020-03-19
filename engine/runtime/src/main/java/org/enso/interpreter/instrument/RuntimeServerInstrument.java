package org.enso.interpreter.instrument;

import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import org.enso.interpeter.instrument.Handler;
import org.enso.polyglot.*;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.polyglot.io.MessageEndpoint;
import org.graalvm.polyglot.io.MessageTransport;

import java.io.IOException;
import java.net.URI;
import java.util.Collections;

/**
 * An instrument exposing a server for other services to connect to, in order to control the current
 * language context and request executions.
 *
 * <p>This architecture ensures class path separation, where the polyglot clients do not depend on
 * this instrument directly, but rather use message passing to interact with it. This is the
 * officially recommended way of handling such interactions in the Truffle framework.
 */
@TruffleInstrument.Registration(
    id = RuntimeServerInfo.INSTRUMENT_NAME,
    services = RuntimeServerInstrument.class)
public class RuntimeServerInstrument extends TruffleInstrument {
  private Handler handler;

  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    Handler handler = new Handler();
    this.handler = handler;

    try {
      MessageEndpoint client =
          env.startServer(URI.create(RuntimeServerInfo.URI), handler.endpoint());
      if (client != null) {
        handler.endpoint().setClient(client);
      }
    } catch (MessageTransport.VetoException | IOException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  protected void onDispose(Env env) {
    if (handler != null) {
      try {
        handler.endpoint().client().sendClose();
      } catch (IOException e) {
        env.getLogger(RuntimeServerInstrument.class)
            .warning("Sending close message to the client failed, because of: " + e.getMessage());
      }
    }
    super.onDispose(env);
  }

  @Override
  protected OptionDescriptors getOptionDescriptors() {
    return OptionDescriptors.create(
        Collections.singletonList(
            OptionDescriptor.newBuilder(new OptionKey<>(""), RuntimeServerInfo.ENABLE_OPTION)
                .build()));
  }

  /**
   * Gets the associated message handler.
   *
   * @return the message handler.
   */
  public Handler getHandler() {
    return handler;
  }
}
