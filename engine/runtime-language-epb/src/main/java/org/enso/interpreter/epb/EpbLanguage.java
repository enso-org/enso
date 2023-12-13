package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.source.Source;
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
    ForeignEvalNode foreignEvalNode =
        ForeignEvalNode.build(this, request.getSource(), request.getArgumentNames());
    return foreignEvalNode.getCallTarget();
  }

  static String truffleId(Source langAndCode) {
    var seq = langAndCode.getCharacters();
    return seq.subSequence(0, splitAt(seq)).toString().toLowerCase();
  }

  private static int splitAt(CharSequence seq) {
    var at = 0;
    while (seq.charAt(at) != '#') {
      at++;
    }
    return at;
  }

  static String foreignSource(Source langAndCode) {
    var seq = langAndCode.getCharacters();
    return seq.toString().substring(splitAt(seq) + 1);
  }

  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return true;
  }
}
