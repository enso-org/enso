package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
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
import com.oracle.truffle.api.source.SourceSection;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.WeakHashMap;
import java.util.logging.Level;

import org.enso.compiler.ModuleCache;
import org.enso.compiler.context.SimpleUpdate;
import org.enso.compiler.core.IR;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.builtin.BuiltinFunction;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.text.buffer.Rope;
import scala.Function1;

/** Represents a source module with a known location. */
@ExportLibrary(InteropLibrary.class)
public final class Module implements TruffleObject {

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
  private ModuleSources sources;
  private PatchedModuleValues patchedValues;
  private final Map<Source, Module> allSources = new WeakHashMap<>();
  private final Package<TruffleFile> pkg;
  private CompilationStage compilationStage = CompilationStage.INITIAL;
  private boolean isIndexed = false;
  private IR.Module ir;
  private Map<UUID, IR> uuidsMap;
  private QualifiedName name;
  private final ModuleCache cache;
  private boolean wasLoadedFromCache;
  private boolean hasCrossModuleLinks;
  private final boolean synthetic;
  private List<QualifiedName> directModulesRefs;

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @param sourceFile the module's source file.
   */
  public Module(QualifiedName name, Package<TruffleFile> pkg, TruffleFile sourceFile) {
    this.sources = ModuleSources.NONE.newWith(sourceFile);
    this.pkg = pkg;
    this.name = name;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.synthetic = false;
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
    this.sources = ModuleSources.NONE.newWith(Rope.apply(literalSource));
    this.pkg = pkg;
    this.name = name;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.patchedValues = new PatchedModuleValues(this);
    this.synthetic = false;
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
    this.sources = ModuleSources.NONE.newWith(literalSource);
    this.pkg = pkg;
    this.name = name;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.patchedValues = new PatchedModuleValues(this);
    this.synthetic = false;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   */
  private Module(
      QualifiedName name,
      Package<TruffleFile> pkg,
      boolean synthetic,
      Rope literalSource,
      EnsoContext context) {
    this.sources =
        literalSource == null ? ModuleSources.NONE : ModuleSources.NONE.newWith(literalSource);
    this.name = name;
    this.scope = new ModuleScope(this, context);
    this.pkg = pkg;
    this.compilationStage = synthetic ? CompilationStage.INITIAL : CompilationStage.AFTER_CODEGEN;
    this.cache = new ModuleCache(this);
    this.wasLoadedFromCache = false;
    this.hasCrossModuleLinks = false;
    this.synthetic = synthetic;
  }

  /**
   * Creates an empty module.
   *
   * @param name the qualified name of the newly created module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @return the module with empty scope.
   */
  public static Module empty(QualifiedName name, Package<TruffleFile> pkg, EnsoContext context) {
    return new Module(name, pkg, false, null, context);
  }

  /**
   * Creates a synthetic module which only purpose is to export symbols of other modules.
   *
   * @param name the qualified name of the newly created module.
   * @param pkg the package this module belongs to. May be {@code null}, if the module does not
   *     belong to a package.
   * @param source source of the module declaring exports of the desired modules
   * @return the synthetic module
   */
  public static Module synthetic(
      QualifiedName name, Package<TruffleFile> pkg, Rope source, EnsoContext context) {
    return new Module(name, pkg, true, source, context);
  }

  /** Clears any literal source set for this module. */
  public void unsetLiteralSource() {
    this.disposeInteractive();
    this.sources = this.sources.reset();
    this.compilationStage = CompilationStage.INITIAL;
  }

  /** @return the literal source of this module. */
  public Rope getLiteralSource() {
    return sources.rope();
  }

  /** @return true if this module represents a synthetic (compiler-generated) module */
  public boolean isSynthetic() {
    return synthetic;
  }

  /**
   * Sets new literal sources for the module.
   *
   * @param source the module source.
   */
  public void setLiteralSource(String source) {
    setLiteralSource(Rope.apply(source), null);
  }

  /**
   * Attaches names of submodules that should be accessible from this module
   *
   * @param names fully qualified names of (potentially synthetic) modules' names
   */
  public void setDirectModulesRefs(List<QualifiedName> names) {
    assert directModulesRefs == null;
    directModulesRefs = names;
  }

  /**
   * Return a list of directly referencing submodules of this one, if any.
   *
   * @return a non-null, possibly empty, list of fully qualified names of modules
   */
  public List<QualifiedName> getDirectModulesRefs() {
    if (directModulesRefs == null) {
      return Collections.emptyList();
    }
    return directModulesRefs;
  }

  /**
   * Sets new literal sources for the module. Optionally one can suggest {@link SimpleUpdate}
   * information to perform small {@link PatchedModuleValues patching} of existing AST, IR & co.
   * rather than complete re-parse.
   *
   * @param source the module source.
   * @param update suggested small change in a single literal or {@code null} when complete
   *     replacement with full re-parse shall be done
   * @see PatchedModuleValues
   */
  public void setLiteralSource(Rope source, SimpleUpdate update) {
    if (this.scope != null && update != null) {
      var change = update.ir();
      if (this.patchedValues == null) {
        this.patchedValues = new PatchedModuleValues(this);
      }
      if (patchedValues.simpleUpdate(update)) {
        this.sources = this.sources.newWith(source);
        final Function1<IR.Expression, IR.Expression> fn =
            new Function1<IR.Expression, IR.Expression>() {
              @Override
              public IR.Expression apply(IR.Expression v1) {
                if (v1 == change) {
                  return update.newIr();
                }
                return v1.mapExpressions(this);
              }
            };
        var copy = this.ir.mapExpressions(fn);
        this.ir = copy;
        this.uuidsMap = null;
        return;
      }
    }
    this.sources = this.sources.newWith(source);
    this.compilationStage = CompilationStage.INITIAL;
  }

  /**
   * Sets a source file for the module.
   *
   * @param file the module source file.
   */
  public void setSourceFile(TruffleFile file) {
    this.sources = ModuleSources.NONE.newWith(file);
    this.compilationStage = CompilationStage.INITIAL;
    disposeInteractive();
  }

  /** Cleans supporting data structures for interactive mode. */
  private void disposeInteractive() {
    if (this.patchedValues != null) {
      this.patchedValues.dispose();
      this.patchedValues = null;
    }
  }

  /** @return the location of this module. */
  public String getPath() {
    return sources.getPath();
  }

  /**
   * Parses the module sources. The results of this operation are cached.
   *
   * @param context context in which the parsing should take place
   * @return the scope defined by this module
   */
  public ModuleScope compileScope(EnsoContext context) {
    ensureScopeExists(context);
    if (!compilationStage.isAtLeast(CompilationStage.AFTER_CODEGEN)) {
      try {
        compile(context);
      } catch (IOException ignored) {
      }
    }
    return scope;
  }

  /** Create scope if it does not exist. */
  public void ensureScopeExists(EnsoContext context) {
    if (scope == null) {
      scope = new ModuleScope(this, context);
      compilationStage = CompilationStage.INITIAL;
    }
  }

  /**
   * @return The truffle-wrapped sources of this module.
   * @throws IOException when the source comes from a file that can't be read.
   */
  public Source getSource() throws IOException {
    final Source cached = sources.source();
    if (cached != null) {
      return cached;
    }
    ModuleSources newSources = sources.ensureSource(name);
    sources = newSources;
    allSources.put(newSources.source(), this);
    return newSources.source();
  }

  /**
   * Constructs source section for current {@link #getSource()} of this module.
   *
   * @param sourceStartIndex 0-based offset at the time compilation was performed
   * @param sourceLength length at the time compilation was performed
   * @return
   */
  public final SourceSection createSection(int sourceStartIndex, int sourceLength) {
    var src = sources.source();
    if (src == null) {
      return null;
    }
    allSources.put(src, this);
    var startDelta = patchedValues == null ? 0 : patchedValues.findDelta(sourceStartIndex, false);
    var endDelta =
        patchedValues == null ? 0 : patchedValues.findDelta(sourceStartIndex + sourceLength, true);
    var start = sourceStartIndex + startDelta;
    var length = sourceLength + endDelta - startDelta;
    if (start + length == src.getLength() + 1) {
      length--;
    }
    if (start + length > src.getLength()) {
      return null;
    }
    return src.createSection(start, length);
  }

  /**
   * Check whether given source has ever been associated with this module.
   *
   * @param s source to check
   * @return {@code true} if the source has been created for this module
   */
  public final boolean isModuleSource(Source s) {
    return allSources.containsKey(s);
  }

  private void compile(EnsoContext context) throws IOException {
    ensureScopeExists(context);
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

  public boolean containsUUID(UUID id) {
    var map = uuidsMap;
    if (map == null) {
      var newMap = new HashMap<UUID, IR>();
      var localIr = getIr();
      if (localIr != null) {
        localIr
            .preorder()
            .foreach(
                (v1) -> {
                  if (v1.getExternalId().isDefined()) {
                    newMap.put(v1.getExternalId().get(), v1);
                  }
                  return null;
                });
      }
      uuidsMap = newMap;
      map = newMap;
    }
    return map.containsKey(id);
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
    this.uuidsMap = null;
  }

  /** @return the runtime scope of this module. */
  public ModuleScope getScope() {
    return scope;
  }

  /**
   * Returns the runtime scope of this module that filters out only the requested types. If the list
   * of requested types is empty, returns the unchanged runtime scope.
   *
   * @param types a list of types to include in the scope
   */
  public ModuleScope getScope(List<String> types) {
    if (types.isEmpty()) {
      return scope;
    } else {
      return scope.withTypes(types);
    }
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
    return sources.file();
  }

  /** @return {@code true} if the module is interactive, {@code false} otherwise */
  public boolean isInteractive() {
    return patchedValues != null;
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
      Types.Pair<Type, String> arguments = Types.extractArguments(args, Type.class, String.class);
      Type type = arguments.getFirst();
      String name = arguments.getSecond();

      try {
        return scope.getMethods().get(type).get(name);
      } catch (NullPointerException npe) {
        TruffleLogger logger = TruffleLogger.getLogger(LanguageInfo.ID, Module.class);
        logger.log(
            Level.SEVERE,
            "Failed to get the requested method. Try clearing your IR caches or disabling caching.");
        throw npe;
      }
    }

    private static Type getType(ModuleScope scope, Object[] args)
        throws ArityException, UnsupportedTypeException {
      String name = Types.extractArguments(args, String.class);
      return scope.getTypes().get(name);
    }

    private static Module reparse(Module module, Object[] args, EnsoContext context)
        throws ArityException {
      Types.extractArguments(args);
      module.sources = module.sources.newWith((Rope) null);
      module.disposeInteractive();
      module.wasLoadedFromCache = false;
      try {
        module.compile(context);
      } catch (IOException ignored) {
      }
      return module;
    }

    private static Module setSource(Module module, Object[] args, EnsoContext context)
        throws ArityException, UnsupportedTypeException {
      String source = Types.extractArguments(args, String.class);
      module.setLiteralSource(source);
      return module;
    }

    private static Module setSourceFile(Module module, Object[] args, EnsoContext context)
        throws ArityException, UnsupportedTypeException {
      String file = Types.extractArguments(args, String.class);
      module.setSourceFile(context.getTruffleFile(new File(file)));
      return module;
    }

    private static Type getAssociatedType(ModuleScope scope, Object[] args) throws ArityException {
      Types.extractArguments(args);
      return scope.getAssociatedType();
    }

    private static Object evalExpression(
        ModuleScope scope, Object[] args, EnsoContext context, CallOptimiserNode callOptimiserNode)
        throws ArityException, UnsupportedTypeException {
      String expr = Types.extractArguments(args, String.class);
      Builtins builtins = context.getBuiltins();
      BuiltinFunction eval =
          builtins
              .getBuiltinFunction(
                  builtins.debug(), Builtins.MethodNames.Debug.EVAL, context.getLanguage())
              .orElseThrow();
      CallerInfo callerInfo = new CallerInfo(null, LocalScope.root(), scope);
      return callOptimiserNode.executeDispatch(
          eval.getFunction(),
          callerInfo,
          context.emptyState(),
          new Object[] {builtins.debug(), Text.create(expr)});
    }

    private static Object generateDocs(Module module, EnsoContext context) {
      return context.getCompiler().generateDocs(module);
    }

    @CompilerDirectives.TruffleBoundary
    private static Object gatherImportStatements(Module module, EnsoContext context) {
      Object[] imports = context.getCompiler().gatherImportStatements(module);
      return new Array(imports);
    }

    @CompilerDirectives.TruffleBoundary
    @Specialization
    static Object doInvoke(
        Module module,
        String member,
        Object[] arguments,
        @Cached LoopingCallOptimiserNode callOptimiserNode)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      EnsoContext context = EnsoContext.get(null);
      ModuleScope scope;
      switch (member) {
        case MethodNames.Module.GET_NAME:
          return module.getName().toString();
        case MethodNames.Module.GET_METHOD:
          scope = module.compileScope(context);
          Function result = getMethod(scope, arguments);
          return result == null ? context.getBuiltins().nothing() : result;
        case MethodNames.Module.GET_TYPE:
          scope = module.compileScope(context);
          return getType(scope, arguments);
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
        case MethodNames.Module.GET_ASSOCIATED_TYPE:
          scope = module.compileScope(context);
          return getAssociatedType(scope, arguments);
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
        || member.equals(MethodNames.Module.REPARSE)
        || member.equals(MethodNames.Module.SET_SOURCE)
        || member.equals(MethodNames.Module.SET_SOURCE_FILE)
        || member.equals(MethodNames.Module.GET_ASSOCIATED_TYPE)
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
        MethodNames.Module.REPARSE,
        MethodNames.Module.SET_SOURCE,
        MethodNames.Module.SET_SOURCE_FILE,
        MethodNames.Module.GET_ASSOCIATED_TYPE,
        MethodNames.Module.EVAL_EXPRESSION);
  }

  @Override
  public String toString() {
    return "Module[" + name + ']';
  }
}
