package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import org.enso.interpreter.epb.node.ContextRewrapNode;
import org.enso.interpreter.epb.node.ForeignEvalNode;

/**
 * An internal language that serves as a bridge between Enso and other supported languages.
 *
 * <p>Truffle places a lot of emphasis on safety guarantees, which also means that single-threaded
 * languages cannot easily be called from multiple threads. We circumvent this by using two separate
 * {@link com.oracle.truffle.api.TruffleContext}s, one (often referred to as "outer") is allowed to
 * run Enso, Host Java, and possibly other thread-ready languages. Languages that cannot safely run
 * in a multithreaded environment are relegated to the other context (referred to as "inner"). The
 * inner context provides a GIL capability, ensuring that access to the single-threaded languages is
 * serialized.
 *
 * <p>This imposes certain limitations on data interchange between the contexts. In particular, it
 * is impossible to execute origin language's code when executing in the other context. Therefore
 * outer context values need to be specially wrapped before being passed (e.g. as arguments) to the
 * inner context, and inner context values need rewrapping for use in the outer context. See {@link
 * org.enso.interpreter.epb.runtime.PolyglotProxy} and {@link
 * ContextRewrapNode} for details of how and when this wrapping is done.
 *
 * <p>With the structure outlined above, EPB is the only language that is initialized in both inner
 * and outer contexts and thus it is very minimal. Its only role is to manage both contexts and
 * provide context-switching facilities.
 */
@TruffleLanguage.Registration(
    id = EpbLanguage.ID,
    name = "Enso Polyglot Bridge",
    characterMimeTypes = {EpbLanguage.MIME},
    internal = true,
    defaultMimeType = EpbLanguage.MIME,
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED)
public class EpbLanguage extends TruffleLanguage<EpbContext> {
  public static final String ID = "epb";
  public static final String MIME = "application/epb";

  @Override
  protected EpbContext createContext(Env env) {
    return new EpbContext(env);
  }

  @Override
  protected void initializeContext(EpbContext context) {
    context.initialize();
  }

  @Override
  protected CallTarget parse(ParsingRequest request) {
    EpbParser.Result code = EpbParser.parse(request.getSource());
    return Truffle.getRuntime()
        .createCallTarget(ForeignEvalNode.build(this, code, request.getArgumentNames()));
  }

  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return true;
  }
}
