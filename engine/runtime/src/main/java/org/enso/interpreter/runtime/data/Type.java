package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.atom.ConstantNode;
import org.enso.interpreter.node.expression.atom.GetFieldNode;
import org.enso.interpreter.node.expression.atom.GetFieldWithMatchNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

import java.util.*;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
public class Type implements TruffleObject {
  private final String name;
  private @CompilerDirectives.CompilationFinal ModuleScope definitionScope;
  private final boolean builtin;
  private final Type supertype;
  private boolean gettersGenerated;

  public Type(String name, ModuleScope definitionScope, Type supertype, boolean builtin) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.supertype = supertype;
    this.builtin = builtin;
    generateQualifiedAccessor();
  }

  private void generateQualifiedAccessor() {
    var node = new ConstantNode(null, this);
    var callTarget = Truffle.getRuntime().createCallTarget(node);
    var function =
        new Function(
            callTarget,
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
      generateQualifiedAccessor();
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
    return supertype;
  }

  public void generateGetters(Language language, List<AtomConstructor> constructors) {
    if (gettersGenerated) return;
    gettersGenerated = true;
    var roots = new HashMap<String, RootNode>();
    if (constructors.size() != 1) {
      var names = new HashMap<String, List<GetFieldWithMatchNode.GetterPair>>();
      constructors.forEach(
          cons -> {
            Arrays.stream(cons.getFields())
                .forEach(
                    field -> {
                      var items = names.computeIfAbsent(field.getName(), (k) -> new ArrayList<>());
                      items.add(new GetFieldWithMatchNode.GetterPair(cons, field.getPosition()));
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
      var cons = constructors.get(0);
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
          RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(node);
          var f =
              new Function(
                  callTarget,
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
    // TODO[MK] make this the eigentype when implementing statics
    return this;
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return name;
  }

  @ExportMessage
  boolean isNull(@CachedLibrary("this") InteropLibrary self) {
    return this == Context.get(self).getBuiltins().nothing();
  }

  @Override
  public String toString() {
    return toDisplayString(true);
  }
}
