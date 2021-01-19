package org.enso.interpreter;

import com.oracle.truffle.api.*;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.Collections;

import org.enso.interpreter.instrument.IdExecutionInstrument;
import org.enso.interpreter.node.ProgramRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.service.ExecutionService;
import org.enso.interpreter.util.FileDetector;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.options.OptionDescriptors;

/**
 * The root of the Enso implementation.
 *
 * <p>This class contains all of the services needed by a Truffle language to enable interoperation
 * with other guest languages on the same VM. This ensures that Enso is usable via the polyglot API,
 * and hence that it can both call other languages seamlessly, and be called from other languages.
 *
 * <p>See {@link TruffleLanguage} for more information on the lifecycle of a language.
 */
@TruffleLanguage.Registration(
    id = LanguageInfo.ID,
    name = LanguageInfo.NAME,
    implementationName = LanguageInfo.IMPLEMENTATION,
    version = LanguageInfo.VERSION,
    defaultMimeType = LanguageInfo.MIME_TYPE,
    characterMimeTypes = {LanguageInfo.MIME_TYPE},
    contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
    fileTypeDetectors = FileDetector.class,
    services = ExecutionService.class)
@ProvidedTags({
  DebuggerTags.AlwaysHalt.class,
  StandardTags.CallTag.class,
  StandardTags.ExpressionTag.class,
  StandardTags.RootTag.class,
  StandardTags.TryBlockTag.class,
  IdentifiedTag.class
})
public final class Language extends TruffleLanguage<Context> {
  private IdExecutionInstrument idExecutionInstrument;

  /**
   * Creates a new Enso context.
   *
   * <p>This method is meant to be fast, and hence should not perform any long-running logic.
   *
   * @param env the language execution environment
   * @return a new Enso context
   */
  @Override
  protected Context createContext(Env env) {
    Context context = new Context(this, getLanguageHome(), env);
    InstrumentInfo idValueListenerInstrument =
        env.getInstruments().get(IdExecutionInstrument.INSTRUMENT_ID);
    idExecutionInstrument = env.lookup(idValueListenerInstrument, IdExecutionInstrument.class);
    env.registerService(new ExecutionService(context, idExecutionInstrument));
    return context;
  }

  /**
   * Initialize the context.
   *
   * @param context the language context
   */
  @Override
  protected void initializeContext(Context context) throws Exception {
    context.initialize();
  }

  /**
   * Checks if this Enso execution environment is accessible in a multithreaded context.
   *
   * @param thread the thread to check access for
   * @param singleThreaded whether or not execution is single threaded
   * @return whether or not thread access is allowed
   */
  @Override
  protected boolean isThreadAccessAllowed(Thread thread, boolean singleThreaded) {
    return true;
  }

  /**
   * Parses Enso source code ready for execution.
   *
   * @param request the source to parse, plus contextual information
   * @return a ready-to-execute node representing the code provided in {@code request}
   */
  @Override
  protected CallTarget parse(ParsingRequest request) {
    RootNode root = ProgramRootNode.build(this, request.getSource());
    return Truffle.getRuntime().createCallTarget(root);
  }

  /** {@inheritDoc} */
  @Override
  protected OptionDescriptors getOptionDescriptors() {
    return RuntimeOptions.OPTION_DESCRIPTORS;
  }

  /**
   * Returns the top scope of the requested context.
   *
   * @param context the context holding the top scope.
   * @return a singleton collection containing the context's top scope.
   */
  @Override
  protected Iterable<Scope> findTopScopes(Context context) {
    return Collections.singleton(context.getTopScope().getScope());
  }

  /** @return a reference to the execution instrument */
  public IdExecutionInstrument getIdExecutionInstrument() {
    return idExecutionInstrument;
  }
}
