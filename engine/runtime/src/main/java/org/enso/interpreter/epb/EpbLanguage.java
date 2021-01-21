package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import org.enso.interpreter.epb.node.SafeEvalNode;
import org.enso.interpreter.epb.node.SafeEvalNodeGen;

@TruffleLanguage.Registration(
    id = "epb",
    name = "Enso Polyglot Bridge",
    characterMimeTypes = {"application/epb"},
    defaultMimeType = "application/epb",
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED)
public class EpbLanguage extends TruffleLanguage<EpbContext> {
  @Override
  protected EpbContext createContext(Env env) {
    EpbContext ctx = new EpbContext(env);
//    System.out.println(
//        "EPB Context Create " + (ctx.isInner() ? "Inner" : "Outer") + " (" + ctx + ")");
    return ctx;
  }

  @Override
  protected void initializeContext(EpbContext context) {
//    System.out.println("EPB Context Initialize " + (context.isInner() ? "Inner" : "Outer"));
    context.initialize();
  }

  @Override
  protected CallTarget parse(ParsingRequest request) {
//    System.out.println("Parsing EPB Code");
    String src = request.getSource().getCharacters().toString();
    String[] langAndCode = src.split("#", 2);
    return Truffle.getRuntime()
        .createCallTarget(
            SafeEvalNodeGen.create(
                this, langAndCode[0], langAndCode[1], request.getArgumentNames()));
  }

  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return true;
  }
}
