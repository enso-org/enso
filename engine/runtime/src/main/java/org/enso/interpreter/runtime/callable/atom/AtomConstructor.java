package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.argument.ReadArgumentNode;
import org.enso.interpreter.node.callable.function.BlockNode;
import org.enso.interpreter.node.expression.atom.GetFieldNode;
import org.enso.interpreter.node.expression.atom.InstantiateNode;
import org.enso.interpreter.node.expression.atom.QualifiedAccessorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

import java.util.Map;

/** A representation of an Atom constructor. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
public final class AtomConstructor implements TruffleObject {

  private final String name;
  private @CompilerDirectives.CompilationFinal ModuleScope definitionScope;
  private final boolean builtin;
  private @CompilerDirectives.CompilationFinal Atom cachedInstance;
  private @CompilerDirectives.CompilationFinal Function constructorFunction;

  /**
   * Creates a new Atom constructor for a given name. The constructor is not valid until {@link
   * AtomConstructor#initializeFields(LocalScope,ExpressionNode[],ExpressionNode[],ArgumentDefinition...)}
   * is called.
   *
   * @param name the name of the Atom constructor
   * @param definitionScope the scope in which this constructor was defined
   */
  public AtomConstructor(String name, ModuleScope definitionScope) {
    this(name, definitionScope, false);
  }

  /**
   * Creates a new Atom constructor for a given name. The constructor is not valid until {@link
   * AtomConstructor#initializeFields(LocalScope,ExpressionNode[],ExpressionNode[],ArgumentDefinition...)}
   * is called.
   *
   * @param name the name of the Atom constructor
   * @param definitionScope the scope in which this constructor was defined
   * @param builtin if true, the constructor refers to a builtin type (annotated with @BuiltinType
   */
  public AtomConstructor(String name, ModuleScope definitionScope, boolean builtin) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.builtin = builtin;
  }

  public boolean isInitialized() {
    return constructorFunction != null;
  }

  public boolean isBuiltin() {
    return builtin;
  }

  public void setShadowDefinitions(ModuleScope scope) {
    if (builtin) {
      // Ensure that synthetic methods, such as getters for fields are in the scope
      // Some scopes won't have any methods at this point, e.g., Nil or Nothing, hence the null
      // check.
      CompilerAsserts.neverPartOfCompilation();
      Map<String, Function> methods = this.definitionScope.getMethods().get(this);
      if (methods != null) {
        methods.forEach((name, fun) -> scope.registerMethod(this, name, fun));
      }
      this.definitionScope = scope;
    } else {
      throw new RuntimeException(
          "Attempting to modify scope of a non-builtin type post-construction is not allowed");
    }
  }

  /**
   * Generates a constructor function for this {@link AtomConstructor}. Note that such manually
   * constructed argument definitions must not have default arguments.
   *
   * @return {@code this}, for convenience
   */
  public AtomConstructor initializeFields(ArgumentDefinition... args) {
    ExpressionNode[] reads = new ExpressionNode[args.length];
    for (int i = 0; i < args.length; i++) {
      reads[i] = ReadArgumentNode.build(i, null);
    }
    return initializeFields(LocalScope.root(), new ExpressionNode[0], reads, args);
  }

  /**
   * Sets the fields of this {@link AtomConstructor} and generates a constructor function.
   *
   * @param localScope a description of the local scope
   * @param assignments the expressions that evaluate and assign constructor arguments to local vars
   * @param varReads the expressions that read field values from local vars
   * @param args the arguments this constructor will take
   * @return {@code this}, for convenience
   */
  public AtomConstructor initializeFields(
      LocalScope localScope,
      ExpressionNode[] assignments,
      ExpressionNode[] varReads,
      ArgumentDefinition... args) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.constructorFunction = buildConstructorFunction(localScope, assignments, varReads, args);
    generateMethods(args);
    if (args.length == 0) {
      cachedInstance = new Atom(this);
    } else {
      cachedInstance = null;
    }
    return this;
  }

  /**
   * Generates a constructor function to be used for object instantiation from other Enso code.
   * Building constructor function involves storing the argument in a local var and then reading it
   * again on purpose. That way default arguments can refer to previously defined constructor
   * arguments.
   *
   * @param localScope a description of the local scope
   * @param assignments the expressions that evaluate and assign constructor arguments to local vars
   * @param varReads the expressions that read field values from previously evaluated local vars
   * @param args the argument definitions for the constructor function to take
   * @return a {@link Function} taking the specified arguments and returning an instance for this
   *     {@link AtomConstructor}
   */
  private Function buildConstructorFunction(
      LocalScope localScope,
      ExpressionNode[] assignments,
      ExpressionNode[] varReads,
      ArgumentDefinition[] args) {

    ExpressionNode instantiateNode = InstantiateNode.build(this, varReads);
    BlockNode instantiateBlock = BlockNode.buildSilent(assignments, instantiateNode);
    RootNode rootNode =
        ClosureRootNode.build(
            null,
            localScope,
            definitionScope,
            instantiateBlock,
            instantiateNode.getSourceSection(),
            definitionScope.getModule().getName().item() + "." + name,
            null,
            false);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
    return new Function(callTarget, null, new FunctionSchema(args));
  }

  private void generateMethods(ArgumentDefinition[] args) {
    generateQualifiedAccessor();
    for (ArgumentDefinition arg : args) {
      definitionScope.registerMethod(this, arg.getName(), generateGetter(arg.getPosition()));
    }
  }

  private void generateQualifiedAccessor() {
    QualifiedAccessorNode node = new QualifiedAccessorNode(null, this);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(node);
    Function function =
        new Function(
            callTarget,
            null,
            new FunctionSchema(
                new ArgumentDefinition(0, "self", ArgumentDefinition.ExecutionMode.EXECUTE)));
    definitionScope.registerMethod(definitionScope.getAssociatedType(), this.name, function);
  }

  private Function generateGetter(int position) {
    GetFieldNode node = new GetFieldNode(null, position);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(node);
    return new Function(
        callTarget,
        null,
        new FunctionSchema(
            new ArgumentDefinition(0, "self", ArgumentDefinition.ExecutionMode.EXECUTE)));
  }

  /**
   * Gets the name of the constructor.
   *
   * @return the name of the Atom constructor
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the scope in which this constructor was defined.
   *
   * @return the scope in which this constructor was defined
   */
  public ModuleScope getDefinitionScope() {
    return definitionScope;
  }

  /**
   * Gets the number of arguments expected by the constructor.
   *
   * @return the number of args expected by the constructor.
   */
  public int getArity() {
    return constructorFunction.getSchema().getArgumentsCount();
  }

  /**
   * Creates a new runtime instance of the Atom represented by this constructor.
   *
   * @param arguments the runtime arguments to the constructor
   * @return a new instance of the atom represented by this constructor
   */
  // TODO [AA] Check where this can be called from user code.
  public Atom newInstance(Object... arguments) {
    if (cachedInstance != null) return cachedInstance;
    return new Atom(this, arguments);
  }

  /**
   * Creates a textual representation of this Atom constructor, useful for debugging.
   *
   * @return a textual representation of this Atom constructor
   */
  @Override
  public String toString() {
    return name;
  }

  /**
   * Gets the constructor function of this constructor.
   *
   * @return the constructor function of this constructor.
   */
  public Function getConstructorFunction() {
    return constructorFunction;
  }

  /**
   * Marks this object as instantiable through the polyglot APIs.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean isInstantiable() {
    return true;
  }

  /**
   * Handles instantiation through the polyglot APIs.
   *
   * @param arguments the field values for the new instance.
   * @return an instance of this constructor with expected fields.
   * @throws ArityException when the provided field count does match this constructor's field count.
   */
  @ExportMessage
  Atom instantiate(Object... arguments) throws ArityException {
    int expected_arity = getArity();
    if (arguments.length != expected_arity) {
      throw ArityException.create(expected_arity, expected_arity, arguments.length);
    }
    if (cachedInstance != null) {
      return cachedInstance;
    }
    return newInstance(arguments);
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return "Constructor<" + name + ">";
  }

  /** @return the fully qualified name of this constructor. */
  public QualifiedName getQualifiedName() {
    if (this == this.getDefinitionScope().getAssociatedType()) {
      return definitionScope.getModule().getName();
    } else {
      return definitionScope.getModule().getName().createChild(getName());
    }
  }

  /** @return the fields defined by this constructor. */
  public ArgumentDefinition[] getFields() {
    return constructorFunction.getSchema().getArgumentInfos();
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor cons, UnresolvedSymbol symbol) {
      return symbol.resolveFor(cons, getContext().getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "self == cachedConstructor",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        AtomConstructor self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("self") AtomConstructor cachedConstructor,
        @Cached("doResolve(cachedConstructor, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(AtomConstructor self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(self, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  boolean canConvertFrom() {
    return true;
  }

  @ExportMessage
  static class GetConversionFunction {
    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(
        AtomConstructor cons, AtomConstructor target, UnresolvedConversion conversion) {
      return conversion.resolveFor(target, cons, getContext().getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "self == cachedConstructor",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        AtomConstructor self,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("self") AtomConstructor cachedConstructor,
        @Cached("doResolve(cachedConstructor, cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        AtomConstructor self, AtomConstructor target, UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(self, target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
