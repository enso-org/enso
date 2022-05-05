package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import org.enso.compiler.ModuleCache;
import org.enso.compiler.core.IR;
import org.enso.compiler.phase.StubIrBuilder;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.text.buffer.Rope;

/** Represents a source module with a known location. */
@ExportLibrary(InteropLibrary.class)
public class Module implements TruffleObject {

  /** Defines a stage of compilation of the module. */
  public enum CompilationStage {
    INITIAL(0),
    AFTER_PARSING(1),
    AFTER_IMPORT_RESOLUTION(2),
    AFTER_GLOBAL_TYPES(3),
    AFTER_STATIC_PASSES(4),
    AFTER_RUNTIME_STUBS(5),
    AFTER_CODEGEN(6);

    private final int ordinal;

    CompilationStage(int ordinal) {
      this.ordinal = ordinal;
    }

    /**
     * Checks whether the current compilation stage is at least as advanced as the provided one.
     *
     * @param stage the stage to compare to.
     * @return whether or not {@code this} is at least as advanced as {@code stage}.
     */
    public boolean isAtLeast(CompilationStage stage) {
      return ordinal >= stage.ordinal;
    }

    /**
     * Checks that the current compilation stage is before the provided one.
     *
     * @param stage the stage to compare to.
     * @return whether or not {@code this} is before then {@code stage}.
     */
    public boolean isBefore(CompilationStage stage) {
      return ordinal < stage.ordinal;
    }
  }

  private ModuleScope scope;
  private TruffleFile sourceFile;
  private Rope literalSource;
  private Source cachedSource;
  private final Package<TruffleFile> pkg;
  private CompilationStage compilationStage = CompilationStage.INITIAL;
  private boolean isIndexed = false;
  private IR.Module ir;
  private QualifiedName name;
  private final ModuleCache cache;
  private boolean wasLoadedFromCache;
  private boolean hasCrossModuleLinks;
  private boolean isInteractive;

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @param sourceFile the module's source file.
   */
  public Module(QualifiedName name, Package<TruffleFile> pkg, TruffleFile sourceFile) {
    this.sourceFile = sourceFile;
    this.pkg = pkg;
    this.name = name;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.isInteractive = false;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @param literalSource the module's source.
   */
  public Module(QualifiedName name, Package<TruffleFile> pkg, String literalSource) {
    this.literalSource = Rope.apply(literalSource);
    this.pkg = pkg;
    this.name = name;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.isInteractive = true;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @param literalSource the module's source.
   */
  public Module(QualifiedName name, Package<TruffleFile> pkg, Rope literalSource) {
    this.literalSource = literalSource;
    this.pkg = pkg;
    this.name = name;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.isInteractive = true;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   */
  private Module(QualifiedName name, Package<TruffleFile> pkg) {
    this.name = name;
    this.scope = new ModuleScope(this);
    this.pkg = pkg;
    this.compilationStage = CompilationStage.AFTER_CODEGEN;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.isInteractive = false;
  }

  /**
   * Creates an empty module.
   *
   * @param name the qualified name of the newly created module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @return the module with empty scope.
   */
  public static Module empty(QualifiedName name, Package<TruffleFile> pkg) {
    return new Module(name, pkg);
  }

  /** Clears any literal source set for this module. */
  public void unsetLiteralSource() {
    this.isInteractive = false;
    this.literalSource = null;
    this.cachedSource = null;
    this.compilationStage = CompilationStage.INITIAL;
  }

  /** @return the literal source of this module. */
  public Rope getLiteralSource() {
    return literalSource;
  }

  /**
   * Sets new literal sources for the module.
   *
   * @param source the module source.
   */
  public void setLiteralSource(String source) {
    setLiteralSource(Rope.apply(source));
  }

  /**
   * Sets new literal sources for the module.
   *
   * @param source the module source.
   */
  public void setLiteralSource(Rope source) {
    this.literalSource = source;
    this.compilationStage = CompilationStage.INITIAL;
    this.cachedSource = null;
    this.isInteractive = true;
  }

  /**
   * Sets a source file for the module.
   *
   * @param file the module source file.
   */
  public void setSourceFile(TruffleFile file) {
    this.literalSource = null;
    this.sourceFile = file;
    this.compilationStage = CompilationStage.INITIAL;
    this.cachedSource = null;
    this.isInteractive = false;
  }

  /** @return the location of this module. */
  public String getPath() {
    if (sourceFile != null) {
      return sourceFile.getPath();
    }
    return null;
  }

  /**
   * Parses the module sources. The results of this operation are cached.
   *
   * @param context context in which the parsing should take place
   * @return the scope defined by this module
   */
  public ModuleScope compileScope(Context context) {
    ensureScopeExists();
    if (!compilationStage.isAtLeast(CompilationStage.AFTER_CODEGEN)) {
      try {
        compile(context);
      } catch (IOException ignored) {
      }
    }
    return scope;
  }

  /** Create scope if it does not exist. */
  public void ensureScopeExists() {
    if (scope == null) {
      scope = new ModuleScope(this);
      compilationStage = CompilationStage.INITIAL;
    }
  }

  /**
   * @return The truffle-wrapped sources of this module.
   * @throws IOException when the source comes from a file that can't be read.
   */
  public Source getSource() throws IOException {
    if (cachedSource != null) {
      return cachedSource;
    }
    if (literalSource != null) {
      cachedSource =
          Source.newBuilder(LanguageInfo.ID, literalSource.characters(), name.toString()).build();
    } else if (sourceFile != null) {
      cachedSource = Source.newBuilder(LanguageInfo.ID, sourceFile).build();
      literalSource = Rope.apply(cachedSource.getCharacters().toString());
    }
    return cachedSource;
  }

  private void compile(Context context) throws IOException {
    ensureScopeExists();
    Source source = getSource();
    if (source == null) return;
    scope.reset();
    compilationStage = CompilationStage.INITIAL;
    context.getCompiler().run(this);
  }

  /** @return IR defined by this module. */
  public IR.Module getIr() {
    return ir;
  }

  /** @return the current compilation stage of this module. */
  public CompilationStage getCompilationStage() {
    return compilationStage;
  }

  /**
   * Sets the current compilation stage of this module.
   *
   * <p>Note that this method should only be used by the {@link org.enso.compiler.Compiler}.
   *
   * @param compilationStage the new compilation stage for the module.
   */
  public void unsafeSetCompilationStage(CompilationStage compilationStage) {
    this.compilationStage = compilationStage;
  }

  /**
   * Sets the IR for this module.
   *
   * <p>Note that the IR should correspond to the current {@link
   * #unsafeSetCompilationStage(CompilationStage) compilation stage} and should only be set by the
   * {@link org.enso.compiler.Compiler}.
   *
   * @param ir the new IR for the module.
   */
  public void unsafeSetIr(IR.Module ir) {
    this.ir = ir;
  }

  /** @return the runtime scope of this module. */
  public ModuleScope getScope() {
    return scope;
  }

  /** @return the qualified name of this module. */
  public QualifiedName getName() {
    return name;
  }

  public Package<TruffleFile> getPackage() {
    return pkg;
  }

  /**
   * Renames a project part of the QualifiedName of this module.
   *
   * @param newName the new project name
   */
  public void renameProject(String newName) {
    this.name = name.renameProject(newName);
  }

  /** @return the indexed flag. */
  public boolean isIndexed() {
    return isIndexed;
  }

  /** Set the indexed flag. */
  public void setIndexed(boolean indexed) {
    isIndexed = indexed;
  }

  /** @return the source file of this module. */
  public TruffleFile getSourceFile() {
    return sourceFile;
  }

  /** @return {@code true} if the module is interactive, {@code false} otherwise */
  public boolean isInteractive() {
    return isInteractive;
  }

  /**
   * Builds an IR stub for this module.
   *
   * <p>Should only be used for source-less modules (e.g. {@link Builtins}).
   */
  public void unsafeBuildIrStub() {
    ir = StubIrBuilder.build(this);
    compilationStage = CompilationStage.AFTER_CODEGEN;
  }

  /** @return the cache for this module */
  public ModuleCache getCache() {
    return cache;
  }

  /** @return {@code true} if the module was loaded from the cache, {@code false} otherwise */
  public boolean wasLoadedFromCache() {
    return wasLoadedFromCache;
  }

  /** @param wasLoadedFromCache whether or not the module was loaded from the cache */
  public void setLoadedFromCache(boolean wasLoadedFromCache) {
    this.wasLoadedFromCache = wasLoadedFromCache;
  }

  /**
   * @return {@code true} if the module has had its cross-module links restored, otherwise {@code
   *     false}
   */
  public boolean hasCrossModuleLinks() {
    return hasCrossModuleLinks;
  }

  /** @param hasCrossModuleLinks whether or not the module has cross-module links restored */
  public void setHasCrossModuleLinks(boolean hasCrossModuleLinks) {
    this.hasCrossModuleLinks = hasCrossModuleLinks;
  }

  /**
   * Handles member invocations through the polyglot API.
   *
   * <p>The exposed members are:
   * <li>{@code get_method(AtomConstructor, String)}
   * <li>{@code get_constructor(String)}
   * <li>{@code get_associated_constructor()}
   * <li>{@code eval_expression(String)}
   */
  @ExportMessage
  abstract static class InvokeMember {
    private static Function getMethod(ModuleScope scope, Object[] args)
        throws ArityException, UnsupportedTypeException {
      Types.Pair<AtomConstructor, String> arguments =
          Types.extractArguments(args, AtomConstructor.class, String.class);
      AtomConstructor cons = arguments.getFirst();
      String name = arguments.getSecond();

      try {
        return scope.getMethods().get(cons).get(name.toLowerCase());
      } catch (NullPointerException npe) {
        TruffleLogger logger = TruffleLogger.getLogger(LanguageInfo.ID, Module.class);
        logger.log(
            Level.SEVERE,
            "Failed to get the requested method. Try clearing your IR caches or disabling caching.");
        throw npe;
      }
    }

    private static AtomConstructor getConstructor(ModuleScope scope, Object[] args)
        throws ArityException, UnsupportedTypeException {
      String name = Types.extractArguments(args, String.class);
      return scope.getConstructors().get(name);
    }

    private static Module reparse(Module module, Object[] args, Context context)
        throws ArityException {
      Types.extractArguments(args);
      module.cachedSource = null;
      module.literalSource = null;
      module.isInteractive = false;
      module.wasLoadedFromCache = false;
      try {
        module.compile(context);
      } catch (IOException ignored) {
      }
      return module;
    }

    private static Module setSource(Module module, Object[] args, Context context)
        throws ArityException, UnsupportedTypeException {
      String source = Types.extractArguments(args, String.class);
      module.setLiteralSource(source);
      return module;
    }

    private static Module setSourceFile(Module module, Object[] args, Context context)
        throws ArityException, UnsupportedTypeException {
      String file = Types.extractArguments(args, String.class);
      module.setSourceFile(context.getTruffleFile(new File(file)));
      return module;
    }

    private static AtomConstructor getAssociatedConstructor(ModuleScope scope, Object[] args)
        throws ArityException {
      Types.extractArguments(args);
      return scope.getAssociatedType();
    }

    private static Object evalExpression(
        ModuleScope scope, Object[] args, Context context, CallOptimiserNode callOptimiserNode)
        throws ArityException, UnsupportedTypeException {
      String expr = Types.extractArguments(args, String.class);
      Builtins builtins = context.getBuiltins();
      Function eval =
          builtins
              .getBuiltinFunction(
                  builtins.debug(), Builtins.MethodNames.Debug.EVAL, context.getLanguage())
              .orElseThrow();
      CallerInfo callerInfo = new CallerInfo(null, LocalScope.root(), scope);
      Object state = context.getBuiltins().nothing().newInstance();
      return callOptimiserNode
          .executeDispatch(
              eval, callerInfo, state, new Object[] {builtins.debug(), Text.create(expr)})
          .getValue();
    }

    private static Object generateDocs(Module module, Context context) {
      return context.getCompiler().generateDocs(module);
    }

    private static Object gatherImportStatements(Module module, Context context) {
      Object[] imports = context.getCompiler().gatherImportStatements(module);
      return new Array(imports);
    }

    @Specialization
    static Object doInvoke(
        Module module,
        String member,
        Object[] arguments,
        @Cached LoopingCallOptimiserNode callOptimiserNode)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      Context context = Context.get(null);
      ModuleScope scope;
      switch (member) {
        case MethodNames.Module.GET_NAME:
          return module.getName().toString();
        case MethodNames.Module.GET_METHOD:
          scope = module.compileScope(context);
          Function result = getMethod(scope, arguments);
          return result == null ? context.getBuiltins().nothing().newInstance() : result;
        case MethodNames.Module.GET_CONSTRUCTOR:
          scope = module.compileScope(context);
          return getConstructor(scope, arguments);
        case MethodNames.Module.REPARSE:
          return reparse(module, arguments, context);
        case MethodNames.Module.GENERATE_DOCS:
          return generateDocs(module, context);
        case MethodNames.Module.GATHER_IMPORT_STATEMENTS:
          return gatherImportStatements(module, context);
        case MethodNames.Module.SET_SOURCE:
          return setSource(module, arguments, context);
        case MethodNames.Module.SET_SOURCE_FILE:
          return setSourceFile(module, arguments, context);
        case MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR:
          scope = module.compileScope(context);
          return getAssociatedConstructor(scope, arguments);
        case MethodNames.Module.EVAL_EXPRESSION:
          scope = module.compileScope(context);
          return evalExpression(scope, arguments, context, callOptimiserNode);
        default:
          throw UnknownIdentifierException.create(member);
      }
    }
  }

  /**
   * Marks the object as having members for the purposes of the polyglot API.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Exposes a member method validity check for the polyglot API.
   *
   * @param member the member to check
   * @return {@code true} if the member is supported, {@code false} otherwise.
   */
  @ExportMessage
  boolean isMemberInvocable(String member) {
    return member.equals(MethodNames.Module.GET_METHOD)
        || member.equals(MethodNames.Module.GET_CONSTRUCTOR)
        || member.equals(MethodNames.Module.REPARSE)
        || member.equals(MethodNames.Module.SET_SOURCE)
        || member.equals(MethodNames.Module.SET_SOURCE_FILE)
        || member.equals(MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR)
        || member.equals(MethodNames.Module.EVAL_EXPRESSION);
  }

  /**
   * Returns a collection of all the supported members in this scope for the polyglot API.
   *
   * @param includeInternal ignored.
   * @return a collection of all the member names.
   */
  @ExportMessage
  Object getMembers(boolean includeInternal) {
    return new Array(
        MethodNames.Module.GET_METHOD,
        MethodNames.Module.GET_CONSTRUCTOR,
        MethodNames.Module.REPARSE,
        MethodNames.Module.SET_SOURCE,
        MethodNames.Module.SET_SOURCE_FILE,
        MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR,
        MethodNames.Module.EVAL_EXPRESSION);
  }
}
