package org.enso.interpreter.arrow;

import static org.enso.interpreter.arrow.ArrowParser.PhysicalLayout.Primitive;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import org.enso.interpreter.arrow.node.ArrowEvalNode;

/** An internal language that implements Arrow specification. */
@TruffleLanguage.Registration(
    id = ArrowLanguage.ID,
    name = "Truffle implementation of Arrow",
    characterMimeTypes = {ArrowLanguage.MIME},
    defaultMimeType = ArrowLanguage.MIME,
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED)
public class ArrowLanguage extends TruffleLanguage<ArrowContext> {

  public static final String ID = "arrow";
  public static final String MIME = "application/arrow";

  public ArrowLanguage() {}

  @Override
  protected ArrowContext createContext(TruffleLanguage.Env env) {
    var ctx = new ArrowContext(env);
    return ctx;
  }

  @Override
  protected void initializeContext(ArrowContext context) {
    context.initialize();
  }

  @Override
  protected CallTarget parse(ParsingRequest request) {
    ArrowParser.Result code = ArrowParser.parse(request.getSource());
    if (code != null) {
      switch (code.getPhysicalLayout()) {
        case Primitive:
          ArrowEvalNode node = ArrowEvalNode.build(this, code);
          return node.getCallTarget();
        default:
          return null;
      }
    } else {
      throw new IllegalArgumentException(
          "unable to parse the code: " + request.getSource().getCharacters().toString());
    }
  }

  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return true;
  }
}
