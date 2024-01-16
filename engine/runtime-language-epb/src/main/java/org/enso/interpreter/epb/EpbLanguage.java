package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import java.util.function.Consumer;

/** An internal language that serves as a bridge between Enso and other supported languages. */
@TruffleLanguage.Registration(
    id = "epb",
    name = "Enso Polyglot Bridge",
    characterMimeTypes = {EpbLanguage.MIME},
    internal = true,
    defaultMimeType = EpbLanguage.MIME,
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
    services = Consumer.class)
public final class EpbLanguage extends TruffleLanguage<EpbContext> {
  public static final String MIME = "application/epb";

  @Override
  protected EpbContext createContext(Env env) {
    var ctx = new EpbContext(env);
    Consumer<String> init = ctx::initialize;
    env.registerService(init);
    return ctx;
  }

  @Override
  protected void initializeContext(EpbContext context) {
    context.initialize(null);
  }

  @Override
  protected CallTarget parse(ParsingRequest request) {
    var node = ForeignEvalNode.parse(this, request.getSource(), request.getArgumentNames());
    return node.getCallTarget();
  }

  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return true;
  }
}
