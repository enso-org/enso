package org.enso.interpreter;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Objects;

import org.enso.compiler.Compiler;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.exception.CompilationAbortedException;
import org.enso.compiler.exception.UnhandledEntity;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.Environment;
import org.enso.distribution.locking.LockManager;
import org.enso.distribution.locking.ThreadSafeFileLockManager;
import org.enso.interpreter.instrument.NotificationHandler;
import org.enso.interpreter.instrument.NotificationHandler.Forwarder;
import org.enso.interpreter.instrument.NotificationHandler.TextMode$;
import org.enso.interpreter.instrument.Timer;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.ProgramRootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.runtime.tag.Patchable;
import org.enso.interpreter.util.FileDetector;
import org.enso.lockmanager.client.ConnectedLockManager;
import org.enso.logger.masking.MaskingFactory;
import org.enso.polyglot.ForeignLanguage;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.RuntimeOptions;
import org.enso.syntax2.Line;
import org.enso.syntax2.Tree;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionType;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.nodes.ExecutableNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;

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
    contextPolicy = TruffleLanguage.ContextPolicy.EXCLUSIVE,
    dependentLanguages = {ForeignLanguage.ID},
    fileTypeDetectors = FileDetector.class,
    services= { Timer.class, NotificationHandler.Forwarder.class, LockManager.class }
)
@ProvidedTags({
  DebuggerTags.AlwaysHalt.class,
  StandardTags.CallTag.class,
  StandardTags.ExpressionTag.class,
  StandardTags.StatementTag.class,
  StandardTags.RootTag.class,
  StandardTags.RootBodyTag.class,
  StandardTags.TryBlockTag.class,
  IdentifiedTag.class,
  AvoidIdInstrumentationTag.class,
  Patchable.Tag.class
})
public final class EnsoLanguage extends TruffleLanguage<EnsoContext> {
  private static final LanguageReference<EnsoLanguage> REFERENCE =
      LanguageReference.create(EnsoLanguage.class);

  public static EnsoLanguage get(Node node) {
    return REFERENCE.get(node);
  }

  /**
   * Creates a new Enso context.
   *
   * <p>This method is meant to be fast, and hence should not perform any long-running logic.
   *
   * @param env the language execution environment
   * @return a new Enso context
   */
  @Override
  protected EnsoContext createContext(Env env) {
    boolean logMasking = env.getOptions().get(RuntimeOptions.LOG_MASKING_KEY);
    MaskingFactory.getInstance().setup(logMasking);

    var notificationHandler = new Forwarder();
    boolean isInteractiveMode = env.getOptions().get(RuntimeOptions.INTERACTIVE_MODE_KEY);
    boolean isTextMode = !isInteractiveMode;
    if (isTextMode) {
      notificationHandler.addListener(TextMode$.MODULE$);
    }
    env.registerService(notificationHandler);

    TruffleLogger logger = env.getLogger(EnsoLanguage.class);

    var environment = new Environment() {};
    var distributionManager = new DistributionManager(environment);

    LockManager lockManager;
    ConnectedLockManager connectedLockManager = null;

    if (isInteractiveMode) {
      logger.finest(
          "Detected interactive mode, will try to connect to a lock manager managed by it.");
      connectedLockManager = new ConnectedLockManager();
      lockManager = connectedLockManager;
      env.registerService(connectedLockManager);
    } else {
      logger.finest("Detected text mode, using a standalone lock manager.");
      lockManager = new ThreadSafeFileLockManager(distributionManager.paths().locks());
      env.registerService(lockManager);
    }


    boolean isExecutionTimerEnabled =
        env.getOptions().get(RuntimeOptions.ENABLE_EXECUTION_TIMER_KEY);
    Timer timer = isExecutionTimerEnabled ? new Timer.Nanosecond() : new Timer.Disabled();
    env.registerService(timer);

    EnsoContext context =
        new EnsoContext(
            this, getLanguageHome(), env, notificationHandler, lockManager, distributionManager);

    return context;
  }

  /**
   * Initialize the context.
   *
   * @param context the language context
   */
  @Override
  @SuppressWarnings("unchecked")
  protected void initializeContext(EnsoContext context) {
    context.initialize();
  }

  /**
   * Finalize the context.
   *
   * @param context the language context
   */
  @Override
  protected void finalizeContext(EnsoContext context) {
    context.shutdown();
  }

  @Override
  public void disposeContext(EnsoContext context) {
    super.disposeContext(context);
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
    return root.getCallTarget();
  }

  /**
   * Parses the given Enso source code snippet in {@code request}.
   *
   * Inline parsing does not handle the following expressions:
   * <ul>
   *     <li>Assignments</li>
   *     <li>Imports and exports</li>
   * </ul>
   * When given the aforementioned expressions in the request, {@code null}
   * will be returned.
   *
   * @param request request for inline parsing
   * @throws InlineParsingException if the compiler failed to parse
   * @return An {@link ExecutableNode} representing an AST fragment if the request contains
   *   syntactically correct Enso source, {@code null} otherwise.
   */
  @Override
  protected ExecutableNode parse(InlineParsingRequest request) throws InlineParsingException {
    if (request.getLocation().getRootNode() instanceof EnsoRootNode ensoRootNode) {
      var context = EnsoContext.get(request.getLocation());
      Tree inlineExpr = context.getCompiler().parseInline(request.getSource());
      var undesirableExprTypes = List.of(
          Tree.Assignment.class,
          Tree.Import.class,
          Tree.Export.class
      );
      if (astContainsExprTypes(inlineExpr, undesirableExprTypes)) {
        throw new InlineParsingException(
            "Inline parsing request contains some of undesirable expression types: "
                + undesirableExprTypes
                + "\n"
                + "Parsed expression: \n"
                + inlineExpr.codeRepr(),
            null
        );
      }

      var module = ensoRootNode.getModuleScope().getModule();
      var localScope = ensoRootNode.getLocalScope();
      var outputRedirect = new ByteArrayOutputStream();
      var redirectConfigWithStrictErrors = new CompilerConfig(
          false,
          false,
          true,
          scala.Option.apply(new PrintStream(outputRedirect))
      );
      var moduleContext = new ModuleContext(
        module, redirectConfigWithStrictErrors,
        scala.Option.empty(),
        scala.Option.empty(),
        false,
        scala.Option.empty()
      );
      var inlineContext = new InlineContext(
          moduleContext,
          redirectConfigWithStrictErrors,
          scala.Some.apply(localScope),
          scala.Some.apply(false),
          scala.Option.empty(),
          scala.Option.empty(),
          scala.Option.empty()
      );
      Compiler silentCompiler = context.getCompiler().duplicateWithConfig(redirectConfigWithStrictErrors);
      scala.Option<ExpressionNode> exprNode;
      try {
        exprNode = silentCompiler
            .runInline(
                request.getSource().getCharacters().toString(),
                inlineContext
            );
      } catch (UnhandledEntity e) {
        throw new InlineParsingException("Unhandled entity: " + e.entity(), e);
      } catch (CompilationAbortedException e) {
        assert outputRedirect.toString().lines().count() > 1 : "Expected a header line from the compiler";
        String compilerErrOutput = outputRedirect.toString();
        throw new InlineParsingException(compilerErrOutput, e);
      } finally {
        silentCompiler.shutdown(false);
      }

      if (exprNode.isDefined()) {
        var language = EnsoLanguage.get(exprNode.get());
        return new ExecutableNode(language) {
          @Child
          private ExpressionNode expr;

          @Override
          public Object execute(VirtualFrame frame) {
            if (expr == null) {
              expr = insert(exprNode.get());
            }
            return expr.executeGeneric(frame);
          }
        };
      }
    }
    return null;
  }

  private static final class InlineParsingException extends Exception {
    InlineParsingException(String message, Throwable cause) {
      super(message, cause);
    }
  }

  /**
   * Returns true if the given ast transitively contains any of {@code exprTypes}.
   */
  private boolean astContainsExprTypes(Tree ast, List<Class<? extends Tree>> exprTypes) {
    boolean astMatchesExprType = exprTypes
          .stream()
          .anyMatch(exprType -> exprType.equals(ast.getClass()));
    if (astMatchesExprType) {
      return true;
    } else if (ast instanceof Tree.BodyBlock block) {
      return block
          .getStatements()
          .stream()
          .map(Line::getExpression)
          .filter(Objects::nonNull)
          .anyMatch((Tree expr) -> astContainsExprTypes(expr, exprTypes));
    } else {
      return false;
    }
  }

  @Option(
          name = "ExecutionEnvironment",
          category = OptionCategory.USER,
          help = "The environment for program execution. Defaults to `design`.")
  public static final OptionKey<ExecutionEnvironment> EXECUTION_ENVIRONMENT =
          new OptionKey<>(
                  ExecutionEnvironment.DESIGN, new OptionType<>("ExecutionEnvironment", ExecutionEnvironment::forName));


  private static final OptionDescriptors OPTIONS =
      OptionDescriptors.createUnion(
          new EnsoLanguageOptionDescriptors(), RuntimeOptions.OPTION_DESCRIPTORS);

  /** {@inheritDoc} */
  @Override
  protected OptionDescriptors getOptionDescriptors() {
    return OPTIONS;
  }

  /**
   * Returns the top scope of the requested context.
   *
   * @param context the context holding the top scope
   * @return the language's top scope
   */
  @Override
  protected Object getScope(EnsoContext context) {
    return context.getTopScope();
  }

  /** Conversions of primitive values */
  protected Object getLanguageView(EnsoContext context, Object value) {
    if (value instanceof Boolean b ) {
      var bool = context.getBuiltins().bool();
      return b ? bool.getTrue().newInstance() : bool.getFalse().newInstance();
    }
    return null;
  }
}
