package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.type.Types;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;

import java.util.*;

/** A representation of Enso's per-file top-level scope. */
@ExportLibrary(InteropLibrary.class)
public class ModuleScope implements TruffleObject {
  private final AtomConstructor associatedType;
  private final Map<String, AtomConstructor> constructors = new HashMap<>();
  private final Map<AtomConstructor, Map<String, Function>> methods = new HashMap<>();
  private final Set<ModuleScope> imports = new HashSet<>();

  /**
   * Creates a new object of this class.
   *
   * @param name the name of the newly created module.
   */
  public ModuleScope(String name) {
    associatedType = new AtomConstructor(name, this).initializeFields();
  }

  /**
   * Adds an Atom constructor definition to the module scope.
   *
   * @param constructor the constructor to register
   */
  public void registerConstructor(AtomConstructor constructor) {
    constructors.put(constructor.getName(), constructor);
  }

  /** @return the associated type of this module. */
  public AtomConstructor getAssociatedType() {
    return associatedType;
  }

  /**
   * Looks up a constructor in the module scope.
   *
   * @param name the name of the module binding
   * @return the Atom constructor associated with {@code name}, or {@link Optional#empty()}
   */
  public Optional<AtomConstructor> getConstructor(String name) {
    if (associatedType.getName().equals(name)) {
      return Optional.of(associatedType);
    }
    Optional<AtomConstructor> locallyDefined = Optional.ofNullable(this.constructors.get(name));
    if (locallyDefined.isPresent()) return locallyDefined;
    return imports.stream()
        .map(scope -> scope.getConstructor(name))
        .filter(Optional::isPresent)
        .map(Optional::get)
        .findFirst();
  }

  /**
   * Returns a map of methods defined in this module for a given constructor.
   *
   * @param cons the constructor for which method map is requested
   * @return a map containing all the defined methods by name
   */
  private Map<String, Function> ensureMethodMapFor(AtomConstructor cons) {
    return methods.computeIfAbsent(cons, k -> new HashMap<>());
  }

  private Map<String, Function> getMethodMapFor(AtomConstructor cons) {
    Map<String, Function> result = methods.get(cons);
    if (result == null) {
      return new HashMap<>();
    }
    return result;
  }

  /**
   * Registers a method defined for a given type.
   *
   * @param atom type the method was defined for
   * @param method method name
   * @param function the {@link Function} associated with this definition
   */
  public void registerMethod(AtomConstructor atom, String method, Function function) {
    ensureMethodMapFor(atom).put(method, function);
  }

  /**
   * Looks up the definition for a given type and method name.
   *
   * <p>The resolution algorithm is first looking for methods defined at the constructor definition
   * site (i.e. non-overloads), then looks for methods defined in this scope and finally tries to
   * resolve the method in all dependencies of this module.
   *
   * @param atom type to lookup the method for.
   * @param name the method name.
   * @return the matching method definition or null if not found.
   */
  @CompilerDirectives.TruffleBoundary
  public Function lookupMethodDefinition(AtomConstructor atom, String name) {
    Function definedWithAtom = atom.getDefinitionScope().getMethodMapFor(atom).get(name);
    if (definedWithAtom != null) {
      return definedWithAtom;
    }
    Function definedHere = getMethodMapFor(atom).get(name);
    if (definedHere != null) {
      return definedHere;
    }
    return imports.stream()
        .map(scope -> scope.getMethodMapFor(atom).get(name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  /**
   * Adds a dependency for this module.
   *
   * @param scope the scope of the newly added dependency
   */
  public void addImport(ModuleScope scope) {
    imports.add(scope);
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
      return scope.methods.get(cons).get(name);
    }

    private static AtomConstructor getConstructor(ModuleScope scope, Object[] args)
        throws ArityException, UnsupportedTypeException {
      String name = Types.extractArguments(args, String.class);
      return scope.constructors.get(name);
    }

    private static ModuleScope patch(ModuleScope scope, Object[] args, Context context)
        throws ArityException, UnsupportedTypeException {
      String sourceString = Types.extractArguments(args, String.class);
      Source source =
          Source.newBuilder(LanguageInfo.ID, sourceString, scope.associatedType.getName()).build();
      context.compiler().run(source, scope);
      return scope;
    }

    private static AtomConstructor getAssociatedConstructor(ModuleScope scope, Object[] args)
        throws ArityException {
      Types.extractArguments(args);
      return scope.associatedType;
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
        ModuleScope scope,
        String member,
        Object[] arguments,
        @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> contextRef,
        @Cached(value = "build()", allowUncached = true) CallOptimiserNode callOptimiserNode)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      switch (member) {
        case MethodNames.Module.GET_METHOD:
          return getMethod(scope, arguments);
        case MethodNames.Module.GET_CONSTRUCTOR:
          return getConstructor(scope, arguments);
        case MethodNames.Module.PATCH:
          return patch(scope, arguments, contextRef.get());
        case MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR:
          return getAssociatedConstructor(scope, arguments);
        case MethodNames.Module.EVAL_EXPRESSION:
          return evalExpression(scope, arguments, contextRef.get(), callOptimiserNode);
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
        MethodNames.Module.GET_ASSOCIATED_CONSTRUCTOR,
        MethodNames.Module.EVAL_EXPRESSION);
  }
}
