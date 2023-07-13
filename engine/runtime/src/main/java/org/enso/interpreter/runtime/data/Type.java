package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.*;
import org.enso.interpreter.Constants;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.expression.atom.ConstantNode;
import org.enso.interpreter.node.expression.atom.GetFieldNode;
import org.enso.interpreter.node.expression.atom.GetFieldWithMatchNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
public final class Type implements TruffleObject {
  private final String name;
  private @CompilerDirectives.CompilationFinal ModuleScope definitionScope;
  private final boolean builtin;
  private final Type supertype;
  private final Type eigentype;
  private final Map<String, AtomConstructor> constructors;

  private boolean gettersGenerated;

  private Type(
      String name, ModuleScope definitionScope, Type supertype, Type eigentype, boolean builtin) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.supertype = supertype;
    this.builtin = builtin;
    this.eigentype = Objects.requireNonNullElse(eigentype, this);
    this.constructors = new HashMap<>();
  }

  public static Type createSingleton(
      String name, ModuleScope definitionScope, Type supertype, boolean builtin) {
    var result = new Type(name, definitionScope, supertype, null, builtin);
    result.generateQualifiedAccessor();
    return result;
  }

  public static Type create(
      String name, ModuleScope definitionScope, Type supertype, Type any, boolean builtin) {
    var eigentype = new Type(name + ".type", definitionScope, any, null, builtin);
    var result = new Type(name, definitionScope, supertype, eigentype, builtin);
    result.generateQualifiedAccessor();
    return result;
  }

  private void generateQualifiedAccessor() {
    var node = new ConstantNode(null, this);
    var function =
        new Function(
            node.getCallTarget(),
            null,
            new FunctionSchema(
                new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE)));
    definitionScope.registerMethod(definitionScope.getAssociatedType(), this.name, function);
  }

  public QualifiedName getQualifiedName() {
    if (this == this.getDefinitionScope().getAssociatedType()) {
      return definitionScope.getModule().getName();
    } else {
      return definitionScope.getModule().getName().createChild(getName());
    }
  }

  public void setShadowDefinitions(ModuleScope scope, boolean generateAccessorsInTarget) {
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
      if (generateAccessorsInTarget) {
        generateQualifiedAccessor();
      }
      if (getEigentype() != this) {
        getEigentype().setShadowDefinitions(scope, false);
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
    return definitionScope;
  }

  public boolean isBuiltin() {
    return builtin;
  }

  public Type getSupertype() {
    if (supertype == null) {
      if (builtin) {
        return null;
      }
      var ctx = EnsoContext.get(null);
      return ctx.getBuiltins().any();
    }
    return supertype;
  }

  public void generateGetters(EnsoLanguage language) {
    if (gettersGenerated) return;
    gettersGenerated = true;
    var roots = new HashMap<String, RootNode>();
    if (constructors.size() != 1) {
      var names = new HashMap<String, List<GetFieldWithMatchNode.GetterPair>>();
      constructors
          .values()
          .forEach(
              cons -> {
                Arrays.stream(cons.getFields())
                    .forEach(
                        field -> {
                          var items =
                              names.computeIfAbsent(field.getName(), (k) -> new ArrayList<>());
                          items.add(
                              new GetFieldWithMatchNode.GetterPair(cons, field.getPosition()));
                        });
              });
      names.forEach(
          (name, fields) -> {
            roots.put(
                name,
                new GetFieldWithMatchNode(
                    language, name, this, fields.toArray(new GetFieldWithMatchNode.GetterPair[0])));
          });
    } else {
      var cons = constructors.values().toArray(AtomConstructor[]::new)[0];
      Arrays.stream(cons.getFields())
          .forEach(
              field -> {
                roots.put(
                    field.getName(),
                    new GetFieldNode(language, field.getPosition(), this, field.getName()));
              });
    }
    roots.forEach(
        (name, node) -> {
          var f =
              new Function(
                  node.getCallTarget(),
                  null,
                  new FunctionSchema(
                      new ArgumentDefinition(
                          0,
                          Constants.Names.SELF_ARGUMENT,
                          ArgumentDefinition.ExecutionMode.EXECUTE)));
          definitionScope.registerMethod(this, name, f);
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
    if (isNothing(lib) || !hasMetaParents()) {
      throw UnsupportedMessageException.create();
    }
    assert getSupertype() != null;
    return new Array(getSupertype());
  }

  @ExportMessage
  boolean hasMetaParents() {
    return getSupertype() != null && getSupertype() != this;
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
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

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  Array getMembers(boolean includeInternal) {
    return new Array(constructors.keySet().toArray(Object[]::new));
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  boolean isMemberReadable(String member) {
    return constructors.containsKey(member);
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  Object readMember(String member) throws UnknownIdentifierException {
    var result = constructors.get(member);
    if (result == null) {
      throw UnknownIdentifierException.create(member);
    } else {
      return result;
    }
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

  public void registerConstructor(AtomConstructor constructor) {
    constructors.put(constructor.getName(), constructor);
  }

  public Map<String, AtomConstructor> getConstructors() {
    return constructors;
  }

  private boolean isNothing(Node lib) {
    var b = EnsoContext.get(lib).getBuiltins();
    return this == b.nothing();
  }
}
