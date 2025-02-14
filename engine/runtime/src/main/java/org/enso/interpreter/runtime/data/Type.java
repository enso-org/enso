package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import org.enso.interpreter.Constants;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.ConstantNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.util.CachingSupplier;
import org.enso.pkg.QualifiedName;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
public final class Type extends EnsoObject {

  private final String name;
  private @CompilerDirectives.CompilationFinal ModuleScope.Builder definitionScope;
  private final boolean builtin;
  private final Type supertype;
  private final Type eigentype;
  private final Map<String, AtomConstructor> constructors;
  private final boolean hasAllConstructorsPrivate;

  private boolean gettersGenerated;
  private Map<String, Function> methods;

  private Type(
      String name,
      ModuleScope.Builder definitionScope,
      Type supertype,
      Type eigentype,
      boolean builtin,
      boolean hasAllConstructorsPrivate) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.supertype = supertype;
    this.builtin = builtin;
    this.hasAllConstructorsPrivate = hasAllConstructorsPrivate;
    this.eigentype = Objects.requireNonNullElse(eigentype, this);
    this.constructors = new HashMap<>();
  }

  public static Type createSingleton(
      String name,
      ModuleScope.Builder definitionScope,
      Type supertype,
      boolean builtin,
      boolean hasAllConstructorsPrivate) {
    return new Type(name, definitionScope, supertype, null, builtin, hasAllConstructorsPrivate);
  }

  public static Type create(
      EnsoLanguage lang,
      String name,
      ModuleScope.Builder definitionScope,
      Type supertype,
      Type any,
      boolean builtin,
      boolean hasAllConstructorsPrivate) {
    var eigentype =
        new Type(name + ".type", definitionScope, any, null, builtin, hasAllConstructorsPrivate);
    var result =
        new Type(name, definitionScope, supertype, eigentype, builtin, hasAllConstructorsPrivate);
    result.generateQualifiedAccessor(lang);
    return result;
  }

  public static Type noType() {
    return new Type("null", null, null, null, false, false);
  }

  private void generateQualifiedAccessor(EnsoLanguage lang) {
    assert lang != null;
    var node = new ConstantNode(lang, getDefinitionScope(), this);
    var schemaBldr =
        FunctionSchema.newBuilder()
            .argumentDefinitions(
                new ArgumentDefinition(
                    0, "this", null, null, ArgumentDefinition.ExecutionMode.EXECUTE));
    if (isProjectPrivate()) {
      schemaBldr.projectPrivate();
    }
    var function = new Function(node.getCallTarget(), null, schemaBldr.build());
    definitionScope.registerMethod(
        definitionScope.asModuleScope().getAssociatedType(), this.name, function);
  }

  public QualifiedName getQualifiedName() {
    if (this == this.getDefinitionScope().getAssociatedType()) {
      return definitionScope.getModule().getName();
    } else {
      return definitionScope.getModule().getName().createChild(getName());
    }
  }

  public void setShadowDefinitions(
      EnsoLanguage lang, ModuleScope.Builder scope, boolean generateAccessorsInTarget) {
    if (builtin) {
      // Ensure that synthetic methods, such as getters for fields are in the scope.
      CompilerAsserts.neverPartOfCompilation();
      this.definitionScope.registerAllMethodsOfTypeToScope(this, scope);
      this.definitionScope = scope;
      if (generateAccessorsInTarget) {
        generateQualifiedAccessor(lang);
      }
      if (getEigentype() != this) {
        getEigentype().setShadowDefinitions(lang, scope, false);
      }
    } else {
      throw new RuntimeException(
          "Attempting to modify scope of a non-builtin type post-construction is not allowed");
    }
  }

  public String getName() {
    return name;
  }

  public ModuleScope getDefinitionScope() {
    return definitionScope.asModuleScope();
  }

  public boolean isBuiltin() {
    return builtin;
  }

  /**
   * Right now types cannot be project private. Waiting for implementation of #8835.
   *
   * @return {@code false}
   */
  public final boolean isProjectPrivate() {
    return false;
  }

  /**
   * Returns true iff this type has project-private constructors only. Note that during the
   * compilation, it is ensured by the {@link
   * org.enso.compiler.pass.analyse.PrivateConstructorAnalysis} compiler pass that all the
   * constructors are either public or project-private.
   *
   * @return true iff this type constructors are project-private.
   */
  public boolean hasAllConstructorsPrivate() {
    return hasAllConstructorsPrivate;
  }

  private Type getSupertype() {
    if (supertype == null) {
      if (builtin) {
        return null;
      }
      var ctx = EnsoContext.get(null);
      return ctx.getBuiltins().any();
    }
    return supertype;
  }

  /**
   * All types this type represents including super types.
   *
   * @param ctx contexts to get Any type (common super class) from
   * @return a compilation constant array with all types this type represents
   */
  @ExportMessage.Ignore
  public final Type[] allTypes(EnsoContext ctx) {
    var types = new Type[3];
    var realCount = fillInTypes(this, types, ctx);
    return Arrays.copyOf(types, realCount);
  }

  /**
   * Fills the provided {@code fill} array with all types the {@code self} type can represent. E.g.
   * including super classes.
   *
   * @param self the type to "enroll"
   * @param fill the array to fill
   * @param ctx context to obtain Any type from
   * @return number of types put into the {@code fill} array
   */
  @ExplodeLoop
  private static int fillInTypes(Type self, Type[] fill, EnsoContext ctx) {
    var at = 0;
    while (at < fill.length) {
      fill[at++] = self;
      if (self.supertype == null) {
        if (self.builtin) {
          return at;
        }
        fill[at++] = ctx.getBuiltins().any();
        return at;
      }
      if (self.supertype == ctx.getBuiltins().any()) {
        fill[at++] = ctx.getBuiltins().any();
        return at;
      }
      if (self == self.supertype) {
        return at;
      }
      self = self.supertype;
    }
    throw CompilerDirectives.shouldNotReachHere(invalidInTypes(self));
  }

  @CompilerDirectives.TruffleBoundary
  private static String invalidInTypes(Type self) {
    return "Cannot compute allTypes for " + self;
  }

  public void generateGetters(EnsoLanguage language) {
    if (gettersGenerated) return;
    gettersGenerated = true;
    var roots = AtomConstructor.collectFieldAccessors(language, this);
    roots.forEach(
        (name, node) -> {
          var functionSupplier =
              CachingSupplier.wrap(
                  () -> {
                    var schemaBldr =
                        FunctionSchema.newBuilder()
                            .argumentDefinitions(
                                new ArgumentDefinition(
                                    0,
                                    Constants.Names.SELF_ARGUMENT,
                                    null,
                                    null,
                                    ArgumentDefinition.ExecutionMode.EXECUTE));
                    if (hasAllConstructorsPrivate) {
                      schemaBldr.projectPrivate();
                    }
                    var funcSchema = schemaBldr.build();
                    return new Function(node.getCallTarget(), null, funcSchema);
                  });
          definitionScope.registerMethod(this, name, functionSupplier);
        });
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType() {
    return eigentype;
  }

  @ExportMessage
  boolean hasMetaObject(@CachedLibrary("this") InteropLibrary lib) {
    if (isNothing(lib)) {
      return false;
    }
    return true;
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary lib) throws UnsupportedMessageException {
    if (isNothing(lib)) {
      throw UnsupportedMessageException.create();
    }
    return getType();
  }

  @ExportMessage
  Object getMetaParents(@CachedLibrary("this") InteropLibrary lib)
      throws UnsupportedMessageException {
    if (!hasMetaParents(lib)) {
      throw UnsupportedMessageException.create();
    }
    assert getSupertype() != null;
    return ArrayLikeHelpers.wrapEnsoObjects(getSupertype());
  }

  @ExportMessage
  boolean hasMetaParents(@CachedLibrary("this") InteropLibrary lib) {
    if (isNothing(lib)) {
      return false;
    }
    return getSupertype() != null && getSupertype() != this;
  }

  @ExportMessage
  @Override
  public String toDisplayString(boolean allowSideEffects) {
    return name;
  }

  @ExportMessage
  boolean isMetaObject(@CachedLibrary("this") InteropLibrary lib) {
    if (isNothing(lib)) {
      return false;
    }
    return true;
  }

  @ExportMessage
  boolean isMetaInstance(Object instance, @CachedLibrary(limit = "3") TypesLibrary lib)
      throws UnsupportedMessageException {
    var b = EnsoContext.get(lib).getBuiltins();
    if (b.any() == this) {
      return true;
    }
    if (isNothing(lib)) {
      throw UnsupportedMessageException.create();
    }
    var type = lib.getType(instance);
    while (type != null && type != b.any()) {
      if (type == this) {
        return true;
      }
      type = type.getSupertype();
    }
    return false;
  }

  @ExportMessage
  String getMetaSimpleName(@CachedLibrary("this") InteropLibrary lib)
      throws UnsupportedMessageException {
    if (isNothing(lib)) {
      throw UnsupportedMessageException.create();
    }
    return getName();
  }

  @ExportMessage
  @TruffleBoundary
  String getMetaQualifiedName(@CachedLibrary("this") InteropLibrary lib)
      throws UnsupportedMessageException {
    if (isNothing(lib)) {
      throw UnsupportedMessageException.create();
    }
    return getQualifiedName().toString();
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Members are constructors and methods from this type, and eigen type. Not extension methods as
   * they are most likely not in the module scope of this type.
   *
   * @param includeInternal
   * @return
   */
  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  EnsoObject getMembers(boolean includeInternal) {
    if (hasAllConstructorsPrivate) {
      return ArrayLikeHelpers.empty();
    }
    var consNames = constructors.keySet();
    if (!includeInternal) {
      return ArrayLikeHelpers.wrapStrings(consNames.toArray(String[]::new));
    }
    var methodNames = methods().keySet();
    var allNames = new HashSet<String>();
    allNames.addAll(consNames);
    allNames.addAll(methodNames);
    return ArrayLikeHelpers.wrapStrings(allNames.toArray(String[]::new));
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  boolean isMemberReadable(String member) {
    if (hasAllConstructorsPrivate) {
      return false;
    } else {
      return constructors.containsKey(member) || methods().containsKey(member);
    }
  }

  @ExportMessage
  @TruffleBoundary
  boolean isMemberInvocable(String member) {
    return methods().containsKey(member);
  }

  @ExportMessage
  static final class InvokeMember {
    @Specialization(
        guards = {"cachedMember.equals(member)", "func != null"},
        limit = "3")
    static Object doCached(
        Type receiver,
        String member,
        Object[] args,
        @Cached("member") String cachedMember,
        @Cached("findMethod(receiver, member)") Function func,
        @Cached("buildInvokeFuncNode(func)") InvokeFunctionNode invokeFuncNode)
        throws UnsupportedMessageException, UnsupportedTypeException, ArityException {
      var argsWithReceiver = new Object[args.length + 1];
      argsWithReceiver[0] = receiver;
      System.arraycopy(args, 0, argsWithReceiver, 1, args.length);
      return invokeFuncNode.execute(func, null, null, argsWithReceiver);
    }

    @Specialization(replaces = "doCached")
    @TruffleBoundary
    static Object doUncached(
        Type receiver,
        String member,
        Object[] args,
        @CachedLibrary(limit = "3") InteropLibrary interop)
        throws UnsupportedMessageException,
            UnsupportedTypeException,
            ArityException,
            UnknownIdentifierException {
      var method = findMethod(receiver, member);
      if (method == null) {
        throw UnknownIdentifierException.create(member);
      }
      var invokeFuncNode = buildInvokeFuncNode(method);
      return doCached(receiver, member, args, member, method, invokeFuncNode);
    }

    static Function findMethod(Type receiver, String name) {
      return receiver.methods().get(name);
    }

    static InvokeFunctionNode buildInvokeFuncNode(Function func) {
      assert func != null;
      var argumentInfos = func.getSchema().getArgumentInfos();
      var callArgInfos = new CallArgumentInfo[argumentInfos.length];
      for (var i = 0; i < argumentInfos.length; i++) {
        var argInfo = argumentInfos[i];
        var callArgInfo = new CallArgumentInfo(argInfo.getName());
        callArgInfos[i] = callArgInfo;
      }
      return InvokeFunctionNode.build(
          callArgInfos, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE);
    }
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  Object readMember(String member) throws UnknownIdentifierException {
    if (hasAllConstructorsPrivate) {
      throw UnknownIdentifierException.create(member);
    }
    var cons = constructors.get(member);
    if (cons != null) {
      return cons;
    }
    var method = methods().get(member);
    if (method != null) {
      return method;
    }
    throw UnknownIdentifierException.create(member);
  }

  @ExportMessage
  boolean isNull(@CachedLibrary("this") InteropLibrary self) {
    return this == EnsoContext.get(self).getBuiltins().nothing();
  }

  @Override
  public String toString() {
    return toDisplayString(true);
  }

  public Type getEigentype() {
    return eigentype;
  }

  public boolean isEigenType() {
    return eigentype == this;
  }

  /**
   * Registers a constructor in this type.
   *
   * @param constructor The constructor to register in this type.
   */
  public void registerConstructor(AtomConstructor constructor) {
    constructors.put(constructor.getName(), constructor);
    gettersGenerated = false;
  }

  public Map<String, AtomConstructor> getConstructors() {
    return constructors;
  }

  private boolean isNothing(Node lib) {
    var b = EnsoContext.get(lib).getBuiltins();
    return this == b.nothing();
  }

  private Map<String, Function> methods() {
    if (methods == null) {
      CompilerDirectives.transferToInterpreter();
      var allMethods = new HashMap<String, Function>();
      var defScope = definitionScope.asModuleScope();
      var methodsFromThisScope = defScope.getMethodsForType(this);
      if (methodsFromThisScope != null) {
        methodsFromThisScope.forEach(
            func -> {
              var simpleName = simpleFuncName(func);
              allMethods.put(simpleName, func);
            });
      }
      if (eigentype != null) {
        var methodsFromEigenScope = eigentype.getDefinitionScope().getMethodsForType(eigentype);
        if (methodsFromEigenScope != null) {
          methodsFromEigenScope.forEach(
              func -> {
                var simpleName = simpleFuncName(func);
                allMethods.put(simpleName, func);
              });
        }
      }
      methods = allMethods;
    }
    return methods;
  }

  private static String simpleFuncName(Function func) {
    assert func.getName() != null;
    if (func.getName().contains(".")) {
      var items = func.getName().split("\\.");
      return items[items.length - 1];
    }
    return func.getName();
  }
}
