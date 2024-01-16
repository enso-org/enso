package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.enso.compiler.context.LocalScope;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.argument.ReadArgumentNode;
import org.enso.interpreter.node.callable.function.BlockNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.Annotation;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

/**
 * A representation of an {@link Atom} constructor. Use {@link AtomNewInstanceNode} to instantiate
 * instances of this constructor.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class AtomConstructor implements EnsoObject {

  private final String name;
  private final ModuleScope definitionScope;
  private final boolean builtin;
  private @CompilerDirectives.CompilationFinal Atom cachedInstance;
  private @CompilerDirectives.CompilationFinal Function constructorFunction;
  private @CompilerDirectives.CompilationFinal Function accessor;

  private final Lock layoutsLock = new ReentrantLock();
  private @CompilerDirectives.CompilationFinal Layout boxedLayout;
  private Layout[] unboxingLayouts = new Layout[0];

  private final Type type;

  /**
   * Creates a new Atom constructor for a given name.The constructor is not valid until {@link
   * AtomConstructor#initializeFields} is called.
   *
   * @param name the name of the Atom constructor
   * @param definitionScope the scope in which this constructor was defined
   * @param type associated type
   */
  public AtomConstructor(String name, ModuleScope definitionScope, Type type) {
    this(name, definitionScope, type, false);
  }

  /**
   * Creates a new Atom constructor for a given name. The constructor is not valid until {@link
   * AtomConstructor#initializeFields} is called.
   *
   * @param name the name of the Atom constructor
   * @param definitionScope the scope in which this constructor was defined
   * @param type associated type
   * @param builtin if true, the constructor refers to a builtin type (annotated with @BuiltinType
   */
  public AtomConstructor(String name, ModuleScope definitionScope, Type type, boolean builtin) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.type = type;
    this.builtin = builtin;
  }

  /**
   * Is the constructor initialized or not.
   *
   * @return {@code true} if {@link initializeFields} method has already been called
   */
  public boolean isInitialized() {
    return constructorFunction != null;
  }

  boolean isBuiltin() {
    return builtin;
  }

  /**
   * Generates a constructor function for this {@link AtomConstructor}. Note that such manually
   * constructed argument definitions must not have default arguments.
   *
   * @return {@code this}, for convenience
   */
  public AtomConstructor initializeFields(EnsoLanguage language, ArgumentDefinition... args) {
    ExpressionNode[] reads = new ExpressionNode[args.length];
    for (int i = 0; i < args.length; i++) {
      reads[i] = ReadArgumentNode.build(i, null, null);
    }
    return initializeFields(
        language, null, LocalScope.root(), new ExpressionNode[0], reads, new Annotation[0], args);
  }

  /**
   * Sets the fields of this {@link AtomConstructor} and generates a constructor function.
   *
   * @param localScope a description of the local scope
   * @param assignments the expressions that evaluate and assign constructor arguments to local vars
   * @param varReads the expressions that read field values from local vars
   * @return {@code this}, for convenience
   */
  public AtomConstructor initializeFields(
      EnsoLanguage language,
      SourceSection section,
      LocalScope localScope,
      ExpressionNode[] assignments,
      ExpressionNode[] varReads,
      Annotation[] annotations,
      ArgumentDefinition... args) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    assert boxedLayout == null : "Don't initialize twice: " + this.name;
    if (args.length == 0) {
      cachedInstance = BoxingAtom.singleton(this);
    } else {
      cachedInstance = null;
    }
    boxedLayout = Layout.createBoxed(args);
    this.constructorFunction =
        buildConstructorFunction(
            language, section, localScope, assignments, varReads, annotations, args);
    this.accessor = generateQualifiedAccessor(language);
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
      EnsoLanguage language,
      SourceSection section,
      LocalScope localScope,
      ExpressionNode[] assignments,
      ExpressionNode[] varReads,
      Annotation[] annotations,
      ArgumentDefinition[] args) {
    ExpressionNode instantiateNode = InstantiateNode.build(this, varReads);
    if (section != null) {
      instantiateNode.setSourceLocation(section.getCharIndex(), section.getCharLength());
    }
    BlockNode instantiateBlock = BlockNode.buildSilent(assignments, instantiateNode);
    RootNode rootNode =
        MethodRootNode.build(
            language, localScope, definitionScope, instantiateBlock, section, type, name);
    RootCallTarget callTarget = rootNode.getCallTarget();
    return new Function(callTarget, null, new FunctionSchema(annotations, args));
  }

  private Function generateQualifiedAccessor(EnsoLanguage lang) {
    var node = new QualifiedAccessorNode(lang, this);
    var callTarget = node.getCallTarget();
    var function =
        new Function(
            callTarget,
            null,
            new FunctionSchema(
                new ArgumentDefinition(
                    0, "self", null, null, ArgumentDefinition.ExecutionMode.EXECUTE)));
    definitionScope.registerMethod(type.getEigentype(), this.name, function);
    return function;
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
   * Gets the display name of the constructor. If the name is Value or Error will include the type
   * name as well.
   *
   * @return the name to display of the Atom constructor
   */
  @TruffleBoundary
  public String getDisplayName() {
    return name.equals("Value") || name.equals("Error") ? type.getName() + "." + name : name;
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
   * @see AtomNewInstanceNode
   */
  final Atom newInstance(Object... arguments) {
    // package private on purpose
    // use AtomNewInstanceNode to create new instances
    if (cachedInstance != null) {
      return cachedInstance;
    }
    return AtomConstructorInstanceNode.uncached(null, this, arguments);
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
   * Gets the qualified accessor function of this constructor.
   *
   * @return the accessor function of this constructor.
   */
  public Function getAccessorFunction() {
    return accessor;
  }

  /**
   * Extracts constructor from given {@link #getAccessorFunction() accessor function}.
   *
   * @param fn the function to check
   * @return associated constructor or {@code null} if the function isn't {@link
   *     #getAccessorFunction() accessor function}.
   */
  public static AtomConstructor accessorFor(Function fn) {
    if (fn.getCallTarget().getRootNode() instanceof QualifiedAccessorNode node) {
      return node.getAtomConstructor();
    } else {
      return null;
    }
  }

  /**
   * Creates field accessors for all fields in given constructors.
   *
   * @param language the language instance to create getters for
   * @param type type to create accessors for
   * @return map from names to accessor root nodes
   */
  @TruffleBoundary
  public static Map<String, RootNode> collectFieldAccessors(EnsoLanguage language, Type type) {
    var constructors = type.getConstructors().values();
    var roots = new TreeMap<String, RootNode>();
    if (constructors.size() != 1) {
      var names = new TreeMap<String, List<GetFieldWithMatchNode.GetterPair>>();
      for (var cons : constructors) {
        for (var field : cons.getFields()) {
          var items = names.computeIfAbsent(field.getName(), (k) -> new ArrayList<>());
          items.add(new GetFieldWithMatchNode.GetterPair(cons, field.getPosition()));
        }
      }
      for (var entry : names.entrySet()) {
        var name = entry.getKey();
        var fields = entry.getValue();
        roots.put(
            name,
            new GetFieldWithMatchNode(
                language,
                name,
                Type.noType(),
                fields.toArray(new GetFieldWithMatchNode.GetterPair[0])));
      }
    } else {
      var cons = constructors.toArray(AtomConstructor[]::new)[0];
      for (var field : cons.getFields()) {
        var node = new GetFieldNode(language, field.getPosition(), type, field.getName());
        roots.put(field.getName(), node);
      }
    }
    return roots;
  }

  final Layout[] getUnboxingLayouts() {
    return unboxingLayouts;
  }

  final Layout getBoxedLayout() {
    return boxedLayout;
  }

  /**
   * Adds a layout, if the caller knows the latest version of the layouts array. This is verified by
   * checking the layout count they know about. This is enough, because the array is append-only.
   *
   * @param layout the layout to add
   * @param knownLayoutCount the number of layouts the caller knows about
   */
  public void atomicallyAddLayout(Layout layout, int knownLayoutCount) {
    layoutsLock.lock();
    try {
      if (unboxingLayouts.length != knownLayoutCount) {
        // client has outdated information and should re-fetch.
        return;
      }
      var newLayouts = new Layout[unboxingLayouts.length + 1];
      System.arraycopy(unboxingLayouts, 0, newLayouts, 0, unboxingLayouts.length);
      newLayouts[unboxingLayouts.length] = layout;
      unboxingLayouts = newLayouts;
    } finally {
      layoutsLock.unlock();
    }
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
  @TruffleBoundary
  String toDisplayString(boolean allowSideEffects) {
    var sb = new StringBuilder();
    sb.append("Constructor<").append(getDisplayName()).append(">");
    for (var f : getFields()) {
      if (!f.hasDefaultValue()) {
        sb.append(" ").append(f.getName()).append("=_");
      }
    }
    return sb.toString();
  }

  /**
   * @return the fully qualified name of this constructor.
   */
  @TruffleBoundary
  public QualifiedName getQualifiedName() {
    return type.getQualifiedName().createChild(getName());
  }

  /**
   * @return the fully qualified name of constructor type.
   */
  @CompilerDirectives.TruffleBoundary
  public QualifiedName getQualifiedTypeName() {
    return type.getQualifiedName();
  }

  /**
   * Definitions of this constructor fields.
   *
   * @return the fields defined by this constructor.
   */
  public ArgumentDefinition[] getFields() {
    return constructorFunction.getSchema().getArgumentInfos();
  }

  /**
   * Type associated with this constructor.
   *
   * @return type this constructor constructs
   */
  @ExportMessage.Ignore
  public Type getType() {
    return type;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().function();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().function();
  }
}
