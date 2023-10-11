package org.enso.interpreter.instrument;

import org.enso.polyglot.debugger.IdExecutionService;

import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.instrumentation.ContextsListener;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.nodes.LanguageInfo;

import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.Optional;

import org.enso.distribution.locking.LockManager;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.service.ExecutionService;
import org.enso.lockmanager.client.ConnectedLockManager;
import org.enso.polyglot.RuntimeServerInfo;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.polyglot.io.MessageEndpoint;
import org.graalvm.polyglot.io.MessageTransport;

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
  private Env env;
  private Handler handler;
  private EventBinding<Initializer> initializerEventBinding;

  /** @return the handler instance. */
  public Handler getHandler() {
    return handler;
  }

  private void initializeExecutionService(ExecutionService service, TruffleContext context) {
    initializerEventBinding.dispose();
    handler.initializeExecutionService(service, context);
  }

  private static class Initializer implements ContextsListener {
    private final RuntimeServerInstrument instrument;

    public Initializer(RuntimeServerInstrument instrument) {
      this.instrument = instrument;
    }

    @Override
    public void onContextCreated(TruffleContext context) {}

    @Override
    public void onLanguageContextCreated(TruffleContext context, LanguageInfo language) {}

    @Override
    public void onLanguageContextInitialized(TruffleContext context, LanguageInfo language) {
      if (language.getId().equals(org.enso.polyglot.LanguageInfo.ID)) {
        Object token = context.enter(null);
        ExecutionService service;
        try {
          var ctx = EnsoContext.get(null);
          var idExecutionInstrument =
              Optional.ofNullable(
                      instrument.env.getInstruments().get(IdExecutionService.INSTRUMENT_ID))
                  .map(
                      idValueListenerInstrument ->
                          instrument.env.lookup(
                              idValueListenerInstrument, IdExecutionService.class));

          var timer = instrument.env.lookup(language, Timer.class);
          var notificationHandler =
              instrument.env.lookup(language, NotificationHandler.Forwarder.class);
          var connectedLockManager = instrument.env.lookup(language, LockManager.class) instanceof ConnectedLockManager connected ? connected : null;
          service =
              new ExecutionService(
                  ctx, idExecutionInstrument, notificationHandler, connectedLockManager, timer);

        } finally {
          context.leave(null, token);
        }
        instrument.initializeExecutionService(service, context);
      }
    }

    @Override
    public void onLanguageContextFinalized(TruffleContext context, LanguageInfo language) {}

    @Override
    public void onLanguageContextDisposed(TruffleContext context, LanguageInfo language) {}

    @Override
    public void onContextClosed(TruffleContext context) {}
  }

  @Override
  protected void onCreate(Env env) {
    this.env = env;
    env.registerService(this);
    Handler handler = new Handler();
    this.handler = handler;

    try {
      MessageEndpoint client =
          env.startServer(URI.create(RuntimeServerInfo.URI), handler.endpoint());
      if (client != null) {
        handler.endpoint().setClient(client);
      } else {
        env.getLogger(RuntimeServerInstrument.class)
            .warning(
                "The client endpoint has not been initialized. The Runtime "
                    + "Server Instrument may very likely not function properly.");
      }
    } catch (MessageTransport.VetoException | IOException e) {
      throw new RuntimeException(e);
    }

    initializerEventBinding =
        env.getInstrumenter().attachContextsListener(new Initializer(this) {}, true);
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
        Arrays.asList(
            OptionDescriptor.newBuilder(RuntimeServerInfo.ENABLE_OPTION_KEY, RuntimeServerInfo.ENABLE_OPTION)
                .build()));
  }
}
