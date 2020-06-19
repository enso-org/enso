package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
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
import org.enso.compiler.core.IR;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.node.callable.dispatch.LoopingCallOptimiserNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.text.buffer.Rope;

/** Represents a source module with a known location. */
@ExportLibrary(InteropLibrary.class)
public class Module implements TruffleObject {
  private ModuleScope scope;
  private TruffleFile sourceFile;
  private Rope literalSource;
  private boolean isParsed = false;
  private IR ir;
  private final QualifiedName name;

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param sourceFile the module's source file.
   */
  public Module(QualifiedName name, TruffleFile sourceFile) {
    this.sourceFile = sourceFile;
    this.name = name;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param literalSource the module's source.
   */
  public Module(QualifiedName name, String literalSource) {
    this.literalSource = Rope.apply(literalSource);
    this.name = name;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   * @param literalSource the module's source.
   */
  public Module(QualifiedName name, Rope literalSource) {
    this.literalSource = literalSource;
    this.name = name;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   */
  private Module(QualifiedName name) {
    this.name = name;
    this.scope = new ModuleScope(this);
    this.isParsed = true;
  }

  /**
   * Creates an empty module.
   *
   * @param name the qualified name of the newly created module.
   * @return the module with empty scope.
   */
  public static Module empty(QualifiedName name) {
    return new Module(name);
  }

  /** Clears any literal source set for this module. */
  public void unsetLiteralSource() {
    this.literalSource = null;
    this.isParsed = false;
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
    this.isParsed = false;
  }

  /**
   * Sets a source file for the module.
   *
   * @param file the module source file.
   */
  public void setSourceFile(TruffleFile file) {
    this.literalSource = null;
    this.sourceFile = file;
    this.isParsed = false;
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
  public ModuleScope parseScope(Context context) {
    ensureScopeExists(context);
    if (!isParsed) {
      parse(context);
    }
    return scope;
  }

  /**
   * Create scope if it does not exist.
   *
   * @param context the language context.
   */
  private void ensureScopeExists(Context context) {
    if (scope == null) {
      scope = context.createScope(this);
      isParsed = false;
    }
  }

  private void parse(Context context) {
    ensureScopeExists(context);
    context.resetScope(scope);
    isParsed = true;
    if (literalSource != null) {
      Source source =
          Source.newBuilder(LanguageInfo.ID, literalSource.characters(), name.toString()).build();
      ir = context.getCompiler().run(source, scope);
    } else if (sourceFile != null) {
      ir = context.getCompiler().run(sourceFile, scope);
    }
  }

  /** @return IR defined by this module. */
  public IR getIr() {
    return ir;
  }

  /** @return the qualified name of this module. */
  public QualifiedName getName() {
    return name;
  }

  /**
   * Handles member invocations through the polyglot API.
   *
   * <p>The exposed members are:
   * <li>{@code get_method(AtomConstructor, String)}
   * <li>{@code get_constructor(String)}
   * <li>{@code patch(String)}
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
      return scope.getMethods().get(cons).get(name);
    }

    private static AtomConstructor getConstructor(ModuleScope scope, Object[] args)
        throws ArityException, UnsupportedTypeException {
      String name = Types.extractArguments(args, String.class);
      return scope.getConstructors().get(name);
    }

    private static Module patch(Module module, Object[] args, Context context)
        throws ArityException, UnsupportedTypeException {
      ModuleScope scope = module.parseScope(context);
      String sourceString = Types.extractArguments(args, String.class);
      Source source =
          Source.newBuilder(LanguageInfo.ID, sourceString, scope.getAssociatedType().getName())
              .build();
      context.getCompiler().run(source, scope);
      return module;
    }

    private static Module reparse(Module module, Object[] args, Context context)
        throws ArityException {
      Types.extractArguments(args);
      module.parse(context);
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
      AtomConstructor debug = context.getBuiltins().debug();
      Function eval =
          context
              .getBuiltins()
              .getScope()
              .lookupMethodDefinition(debug, Builtins.MethodNames.Debug.EVAL);
      CallerInfo callerInfo = new CallerInfo(null, LocalScope.root(), scope);
      Object state = context.getBuiltins().unit().newInstance();
      return callOptimiserNode
          .executeDispatch(eval, callerInfo, state, new Object[] {debug, expr})
          .getValue();
    }

    @Specialization
    static Object doInvoke(
        Module module,
        String member,
        Object[] arguments,
        @CachedContext(Language.class) Context context,
        @Cached LoopingCallOptimiserNode callOptimiserNode)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      ModuleScope scope = module.parseScope(context);
      switch (member) {
        case MethodNames.Module.GET_METHOD:
          return getMethod(scope, arguments);
        case MethodNames.Module.GET_CONSTRUCTOR:
          return getConstructor(scope, arguments);
        case MethodNames.Module.PATCH:
          return patch(module, arguments, context);
        case MethodNames.Module.REPARSE:
          return reparse(module, arguments, context);
        case MethodNames.Module.SET_SOURCE:
          return setSource(module, arguments, context);
        case MethodNames.Module.SET_SOURCE_FILE:
          return setSourceFile(module, arguments, context);
        case MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR:
          return getAssociatedConstructor(scope, arguments);
        case MethodNames.Module.EVAL_EXPRESSION:
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
        || member.equals(MethodNames.Module.PATCH)
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
    return new Vector(
        MethodNames.Module.GET_METHOD,
        MethodNames.Module.GET_CONSTRUCTOR,
        MethodNames.Module.PATCH,
        MethodNames.Module.REPARSE,
        MethodNames.Module.SET_SOURCE,
        MethodNames.Module.SET_SOURCE_FILE,
        MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR,
        MethodNames.Module.EVAL_EXPRESSION);
  }
}
