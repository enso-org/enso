package org.enso.interpreter;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import org.enso.interpreter.builder.FileDetector;
import org.enso.interpreter.builder.GlobalScopeExpressionFactory;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Context;

@TruffleLanguage.Registration(
    id = Constants.LANGUAGE_ID,
    name = Constants.LANGUAGE_NAME,
    implementationName = Constants.IMPL_NAME,
    version = Constants.LANGUAGE_VERSION,
    defaultMimeType = Constants.MIME_TYPE,
    characterMimeTypes = Constants.MIME_TYPE,
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
    fileTypeDetectors = FileDetector.class)
@ProvidedTags({
  StandardTags.CallTag.class,
  StandardTags.ExpressionTag.class,
  StandardTags.RootTag.class,
  StandardTags.TryBlockTag.class
})
public final class Language extends TruffleLanguage<Context> {

  @Override
  protected Context createContext(Env env) {
    return new Context(this, env);
  }

  @Override
  protected boolean isObjectOfLanguage(Object object) {
    return false;
  }

  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return super.isThreadAccessAllowed(thread, singleThreaded);
  }

  @Override
  protected CallTarget parse(ParsingRequest request) throws Exception {
    AstGlobalScope parsed =
        new EnsoParser().parseEnso(request.getSource().getCharacters().toString());
    ExpressionNode result = new GlobalScopeExpressionFactory(this).run(parsed);
    EnsoRootNode root = new EnsoRootNode(this, new FrameDescriptor(), result, null, "root");
    return Truffle.getRuntime().createCallTarget(root);
  }

  public Context getCurrentContext() {
    return getCurrentContext(Language.class);
  }
}
