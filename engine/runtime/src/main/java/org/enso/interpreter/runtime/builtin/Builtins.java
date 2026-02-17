package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import java.io.IOException;
import java.util.Optional;
import org.enso.common.MethodNames;
import org.enso.compiler.Passes;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.phase.BuiltinsIrBuilder;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.expression.builtin.Any;
import org.enso.interpreter.node.expression.builtin.Boolean;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.node.expression.builtin.Nothing;
import org.enso.interpreter.node.expression.builtin.Polyglot;
import org.enso.interpreter.node.expression.builtin.debug.Debug;
import org.enso.interpreter.node.expression.builtin.error.AdditionalWarnings;
import org.enso.interpreter.node.expression.builtin.error.CaughtPanic;
import org.enso.interpreter.node.expression.builtin.error.NoWrap;
import org.enso.interpreter.node.expression.builtin.error.ProblemBehavior;
import org.enso.interpreter.node.expression.builtin.error.Warning;
import org.enso.interpreter.node.expression.builtin.immutable.Vector;
import org.enso.interpreter.node.expression.builtin.meta.ProjectDescription;
import org.enso.interpreter.node.expression.builtin.mutable.Array;
import org.enso.interpreter.node.expression.builtin.mutable.Ref;
import org.enso.interpreter.node.expression.builtin.ordering.Comparable;
import org.enso.interpreter.node.expression.builtin.ordering.DefaultComparator;
import org.enso.interpreter.node.expression.builtin.ordering.Ordering;
import org.enso.interpreter.node.expression.builtin.resource.ManagedResource;
import org.enso.interpreter.node.expression.builtin.runtime.Context;
import org.enso.interpreter.node.expression.builtin.text.Text;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.ModuleScopeBuilder;
import org.enso.interpreter.runtime.data.Type;
import org.enso.pkg.QualifiedName;

/** Container class for static predefined atoms, methods, and their containing scope. */
public final class Builtins {
  private static final EnsoContext.Extra<Builtins> KEY =
      new EnsoContext.Extra<>(Builtins.class, Builtins::create);
  private final EnsoContext context;
  private final BuiltinsRegistry builtins;

  /**
   * Module isn't final, as it wasn't possible to assign it in constructor. But it is "effectively
   * final" - nobody is changing that field.
   */
  @CompilerDirectives.CompilationFinal private Module module;

  private final Error error;
  private final Number number;
  private final Boolean bool;

  private final Context contexts;
  private final Ordering ordering;
  private final Comparable comparable;
  private final DefaultComparator defaultComparator;
  private final System system;

  // Builtin types
  private final Builtin any;
  private final Builtin nothing;
  private final Builtin function;
  private final Builtin polyglot;
  private final Builtin text;
  private final Builtin array;
  private final Builtin vector;
  private final Builtin dictionary;
  private final Builtin dataflowError;
  private final Builtin ref;
  private final Builtin managedResource;
  private final Builtin debug;
  private final ProjectDescription projectDescription;
  private final Builtin date;
  private final Builtin dateTime;
  private final Builtin duration;
  private final Builtin timeOfDay;
  private final Builtin timeZone;
  private final Builtin warning;
  private final NoWrap noWrap;
  private final ProblemBehavior problemBehavior;
  private final AdditionalWarnings additionalWarnings;
  private final Builtin instrumentor;

  /** Factory method to create the builtins. */
  private static Builtins create(EnsoContext context) {
    var fqn = QualifiedName.fromString(MethodNames.Builtins.MODULE_NAME);
    var res = new Builtins[1];
    var module =
        Module.emptyWith(
            fqn,
            null,
            (scopeBuilder) -> {
              res[0] = new Builtins(context, scopeBuilder);
            });
    // module has to be assigned only when constructor is over
    res[0].module = module;
    return res[0];
  }

  /**
   * Creates an instance with builtin methods installed.
   *
   * @param ctx the current {@link EnsoContext} instance
   * @param sb scope builder to fill
   */
  private Builtins(EnsoContext ctx, ModuleScopeBuilder sb) {
    context = ctx;
    builtins = new BuiltinsRegistry(ctx.getLanguage(), sb);

    ordering = getBuiltinType(Ordering.class);
    comparable = getBuiltinType(Comparable.class);
    defaultComparator = getBuiltinType(DefaultComparator.class);
    bool = getBuiltinType(Boolean.class);
    contexts = getBuiltinType(Context.class);

    any = getBuiltinType(Any.class);
    nothing = getBuiltinType(Nothing.class);
    function = getBuiltinType(org.enso.interpreter.node.expression.builtin.function.Function.class);
    polyglot = getBuiltinType(Polyglot.class);
    text = getBuiltinType(Text.class);
    array = getBuiltinType(Array.class);
    vector = getBuiltinType(Vector.class);
    dictionary = getBuiltinType(org.enso.interpreter.node.expression.builtin.Dictionary.class);
    dataflowError = getBuiltinType(org.enso.interpreter.node.expression.builtin.Error.class);
    ref = getBuiltinType(Ref.class);
    managedResource = getBuiltinType(ManagedResource.class);
    debug = getBuiltinType(Debug.class);
    projectDescription = getBuiltinType(ProjectDescription.class);
    date = getBuiltinType(org.enso.interpreter.node.expression.builtin.date.Date.class);
    dateTime = getBuiltinType(org.enso.interpreter.node.expression.builtin.date.DateTime.class);
    duration = getBuiltinType(org.enso.interpreter.node.expression.builtin.date.Duration.class);
    timeOfDay = getBuiltinType(org.enso.interpreter.node.expression.builtin.date.TimeOfDay.class);
    timeZone = getBuiltinType(org.enso.interpreter.node.expression.builtin.date.TimeZone.class);
    warning = getBuiltinType(Warning.class);
    noWrap = getBuiltinType(NoWrap.class);
    problemBehavior = getBuiltinType(ProblemBehavior.class);
    additionalWarnings = getBuiltinType(AdditionalWarnings.class);
    instrumentor = getBuiltinType(org.enso.interpreter.node.expression.builtin.Instrumentor.class);

    error = new Error(this, ctx);
    system = new System(this);
    number = new Number(this);
  }

  /**
   * Obtains instance of {@link Builtins} for given context.
   *
   * @param ctx the context to find builtins for
   * @return the 1:1 instance associated with provided context
   */
  public static Builtins get(EnsoContext ctx) {
    return KEY.get(ctx);
  }

  /**
   * Obtains instance of {@link Builtins} for given node. Uses {@link EnsoContext#get} followed by
   * {@link #get(org.enso.interpreter.runtime.EnsoContext)}.
   *
   * @param node the node to find builtins for
   * @return instance of builtins for given node
   */
  public static Builtins get(Node node) {
    var ctx = EnsoContext.get(node);
    assert ctx != null : "No context for " + node;
    return KEY.get(ctx);
  }

  /**
   * @return {@code true} if the IR has been initialized, otherwise {@code false}
   */
  public boolean isIrInitialized() {
    return this.module.getIr() != null;
  }

  /** Initialize the source file for the builtins module. */
  @CompilerDirectives.TruffleBoundary
  public void initializeBuiltinsSource() {
    module.setLiteralSource("");
  }

  /**
   * Initialize the IR for the builtins module from the builtins source file.
   *
   * @param freshNameSupply the compiler's fresh name supply
   * @param passes the passes manager for the compiler
   */
  @CompilerDirectives.TruffleBoundary
  public void initializeBuiltinsIr(
      CompilerContext context, FreshNameSupply freshNameSupply, Passes passes) {
    try {
      if (module.getSource() == null) {
        initializeBuiltinsSource();
      }
      BuiltinsIrBuilder.build(context, module.asCompilerModule(), freshNameSupply, passes);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Returns a builtin method for the provided Atom Constructor and the name, if it exists.
   *
   * @param type Atom Constructor owner of the function
   * @param methodName Name of the method
   * @param language The language the resulting function nodes should be associated with
   * @return A non-empty function under the given name, if it exists. An empty value if no such
   *     builtin method was ever registerd
   */
  public Optional<BuiltinFunction> getBuiltinFunction(
      String type, String methodName, EnsoLanguage language) {
    return builtins.getBuiltinFunction(type, methodName, language);
  }

  public Optional<BuiltinFunction> getBuiltinFunction(
      Type type, String methodName, EnsoLanguage language) {
    return getBuiltinFunction(type.getName(), methodName, language);
  }

  public <T extends Builtin> T getBuiltinType(Class<T> clazz) {
    return builtins.getBuiltinType(clazz);
  }

  public Builtin getByRepresentationType(Class<?> clazz) {
    return builtins.getByRepresentationType(clazz);
  }

  public Builtin getBuiltinType(String name) {
    return builtins.getBuiltinType(name);
  }

  /**
   * Returns the {@code Nothing} atom constructor.
   *
   * @return the {@code Nothing} atom constructor
   */
  public Type nothing() {
    return nothing.getType();
  }

  /**
   * Returns the {@code Text} part of builtins.
   *
   * @return the {@code Text} part of builtins.
   */
  public Type text() {
    return text.getType();
  }

  /**
   * Returns the {@code Function} atom constructor.
   *
   * @return the {@code Function} atom constructor
   */
  public Type function() {
    return function.getType();
  }

  /**
   * Returns the number-related entities.
   *
   * @return the number-related part of builtins.
   */
  public Number number() {
    return number;
  }

  /**
   * @return the builtin Context type
   */
  public Context context() {
    return contexts;
  }

  /**
   * @return the container for boolean constructors.
   */
  public Boolean bool() {
    return bool;
  }

  /**
   * @return the ManagedResource constructor.
   */
  public Type managedResource() {
    return managedResource.getType();
  }

  /**
   * @return the builtin Error types container.
   */
  public Error error() {
    return error;
  }

  /**
   * Returns the {@code Any} atom constructor.
   *
   * @return the {@code Any} atom constructor
   */
  public Type any() {
    return any.getType();
  }

  /**
   * Returns the {@code Warning} atom constructor.
   *
   * @return the {@code Warning} atom constructor
   */
  public Type warning() {
    return warning.getType();
  }

  /** Returns the {@code Problem_Behavior} type. */
  public ProblemBehavior problemBehavior() {
    return problemBehavior;
  }

  /** Returns the {@code No_Wrap} atom constructor. */
  public NoWrap noWrap() {
    return noWrap;
  }

  /** Returns the {@code Additional_Warnings} atom constructor. */
  public AdditionalWarnings additionalWarnings() {
    return additionalWarnings;
  }

  /**
   * Returns the {@code Date} atom constructor.
   *
   * @return the {@code Date} atom constructor
   */
  public Type date() {
    return date.getType();
  }

  /**
   * Returns the {@code DateTime} atom constructor.
   *
   * @return the {@code DateTime} atom constructor
   */
  public Type dateTime() {
    return dateTime.getType();
  }

  /**
   * Returns the {@code TimeOfDay} atom constructor.
   *
   * @return the {@code TimeOfDay} atom constructor
   */
  public Type timeOfDay() {
    return timeOfDay.getType();
  }

  /**
   * Returns the {@code Duration} atom constructor.
   *
   * @return the {@code Duration} atom constructor.
   */
  public Type duration() {
    return duration.getType();
  }

  /**
   * Returns the {@code TimeZone} atom constructor.
   *
   * @return the {@code TimeZone} atom constructor
   */
  public Type timeZone() {
    return timeZone.getType();
  }

  /**
   * Returns the {@code Debug} atom constructor. TODO: this is redundant, figure out a way to avoid
   * createing spurious Debug builtin type
   *
   * @return the {@code Debug} atom constructor
   */
  public Type debug() {
    return debug.getType();
  }

  /**
   * @return the {@code Project_Description} atom constructor
   */
  public ProjectDescription getProjectDescription() {
    return projectDescription;
  }

  /**
   * @return the {@code System} atom constructor.
   */
  public System system() {
    return system;
  }

  /**
   * @return the Array constructor.
   */
  public Type array() {
    return array.getType();
  }

  public Type vector() {
    return vector.getType();
  }

  public Type dictionary() {
    return dictionary.getType();
  }

  /**
   * @return the Ref constructor.
   */
  public Type ref() {
    return ref.getType();
  }

  /**
   * @return the container for polyglot-related builtins.
   */
  public Type polyglot() {
    return polyglot.getType();
  }

  /**
   * @return the {@code Caught_Panic} atom constructor
   */
  public CaughtPanic caughtPanic() {
    return this.error.caughtPanic();
  }

  /**
   * @return the {@code Panic} atom constructor
   */
  public Type panic() {
    return this.error.panic();
  }

  /**
   * @return the container for ordering-related builtins
   */
  public Ordering ordering() {
    return ordering;
  }

  public Comparable comparable() {
    return comparable;
  }

  public DefaultComparator defaultComparator() {
    return defaultComparator;
  }

  /**
   * @return the container for the dataflow error-related builtins
   */
  public Type dataflowError() {
    return dataflowError.getType();
  }

  /**
   * @return represents the Instrumentor type
   */
  public Type instrumentor() {
    return instrumentor.getType();
  }

  public Module getModule() {
    return module;
  }

  public EnsoLanguage getLanguage() {
    return context.getLanguage();
  }
}
