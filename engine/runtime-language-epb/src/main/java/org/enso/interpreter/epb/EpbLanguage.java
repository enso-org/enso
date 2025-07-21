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
 * Each <em>compliant enough</em> Truffle language implementation that supports {@link
 * TruffleLanguage#parse(com.oracle.truffle.api.TruffleLanguage.ParsingRequest)} with arguments
 * provided via {@link ParsingRequest#getArgumentNames()} can be integrated with Enso's {@code
 * foreign} code snippet concept.
 *
 * <h3><code>foreign js</code> Support</h3>
 *
 * Graal.js is compliant enough, but comes with a <em>single threaded</em> restriction. Thus it
 * needs special support that creates a secondary <em>JavaScript only</em> context in {@link
 * ForeignEvalNode#parseJs()}.
 *
 * <p>Another special support provided to {@code foreign js} code is ability to refer to Enso {@code
 * self} as JavaScript {@code this}. Such a support isn't achievable via standard Truffle means and
 * requires {@link JsForeignNode#doExecute} to use {@code apply} JavaScript function directly.
 *
 * <h3><code>foreign python</code> Support</h3>
 *
 * Graal Python had a lot of limitations in the past and as such the {@link PyForeignNode} does a
 * lot of conversions to make sure Python {@code None} and Enso {@code Nothing} represent the same
 * <em>null value</em>, that dates are properly transferred, etc.
 *
 * <p>Ideally each such limitation is reported to GraalPython guys. List of known issues:
 *
 * <ul>
 *   <li><a href="https://github.com/oracle/graalpython/issues/354">numpy numbers aren't reported as
 *       numbers</a>
 *   <li><a href="https://github.com/oracle/graalpython/issues/353">Pandas DataFrame is reported as
 *       array</a>
 * </ul>
 *
 * Once an official solution is available, workarounds may be removed.
 *
 * <h3><code>polyglot java</code> Support</h3>
 *
 * {@code EpbLanguage} is responsible for handling loading of Java classes via {@link
 * JavaPolyglotNode}. There are three modes to load classes:
 *
 * <ul>
 *   <li>{@code hosted} - regular GraalVM Java interop is used to load JVM classes
 *   <li>{@code guest} - support SubstrateVM/HotspotVM bridge via {@code JVM} and {@code Channel}
 *   <li>Espresso support - experimental controlled by {@code ENSO_JAVA} environment variable
 * </ul>
 *
 * Each of these mechanisms is supposed to create a {@link TruffleObject} representing the JVM class
 * that Enso can then operate with.
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
