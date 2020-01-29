package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
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

import java.io.File;
import java.io.IOException;

/** Represents a source module with a known location. */
@ExportLibrary(InteropLibrary.class)
public class Module implements TruffleObject {
  private ModuleScope scope;
  private TruffleFile sourceFile;
  private Source literalSource;
  private boolean isParsed = false;
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

  public Module(QualifiedName name, Source literalSource) {
    this.literalSource = literalSource;
    this.name = name;
  }

  /**
   * Creates a new module.
   *
   * @param name the qualified name of this module.
   */
  public Module(QualifiedName name, ModuleScope scope) {
    this.name = name;
    this.scope = scope;
    this.isParsed = true;
  }

  public void setLiteralSource(Source source) {
    this.literalSource = source;
    this.sourceFile = null;
    this.isParsed = false;
  }

  public void setSourceFile(TruffleFile file) {
    this.literalSource = null;
    this.sourceFile = file;
    this.isParsed = false;
  }

  /**
   * Parses the module sources. The results of this operation are cached.
   *
   * @param context context in which the parsing should take place
   * @return the scope defined by this module
   */
  public ModuleScope getScope(Context context) {
    ensureScopeExists(context);
    if (!isParsed) {
      parse(context);
    }
    return scope;
  }

  private void ensureScopeExists(Context context) {
    if (scope == null) {
      scope = context.createScope(name.module());
      isParsed = false;
    }
  }

  public void parse(Context context) {
    ensureScopeExists(context);
    context.resetScope(scope);
    isParsed = true;
    if (sourceFile != null) {
      context.compiler().run(sourceFile, scope);
    } else if (literalSource != null) {
      context.compiler().run(literalSource, scope);
    }
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
      ModuleScope scope = module.getScope(context);
      String sourceString = Types.extractArguments(args, String.class);
      Source source =
          Source.newBuilder(LanguageInfo.ID, sourceString, scope.getAssociatedType().getName())
              .build();
      context.compiler().run(source, scope);
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
      module.setLiteralSource(
          Source.newBuilder(LanguageInfo.ID, source, module.name.module()).build());
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
      CallerInfo callerInfo = new CallerInfo(null, new LocalScope(), scope);
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
        @Cached(value = "build()", allowUncached = true) CallOptimiserNode callOptimiserNode)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      ModuleScope scope = module.getScope(context);
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
