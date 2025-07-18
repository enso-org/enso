package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import java.util.function.Consumer;

/**
 * <em>Enso Polyglot Bindings</em> language is an internal language that serves as a bridge between
 * Enso and other supported languages. See <a
 * href="https://github.com/enso-org/enso/blob/develop/docs/polyglot/README.md">polyglot docs</a>
 * for a high level overview of intended behavior. Technical details are provided in this Javadoc
 * and of course in this package code.
 *
 * <h3>Generic <code>foreign</code> Support</h3>
 *
 * TBD
 *
 * <h3><code>foreign js</code> Support</h3>
 *
 * TBD
 *
 * <h3><code>foreign python</code> Support</h3>
 *
 * TBD
 *
 * <h3><code>polyglot java</code> Support</h3>
 *
 * TBD
 */
@TruffleLanguage.Registration(
    id = EpbLanguage.ID,
    name = "Enso Polyglot Bridge",
    characterMimeTypes = {EpbLanguage.MIME},
    internal = true,
    defaultMimeType = EpbLanguage.MIME,
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
    services = Consumer.class)
public final class EpbLanguage extends TruffleLanguage<EpbContext> {
  public static final String ID = "epb";
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
