package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

import java.util.Map;

public class Type implements TruffleObject {
  private final String name;
  private @CompilerDirectives.CompilationFinal ModuleScope definitionScope;
  private final boolean builtin;
  private final Type supertype;

  public Type(String name, ModuleScope definitionScope, Type supertype, boolean builtin) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.supertype = supertype;
    this.builtin = builtin;
  }
  //
  //  public static Type create(
  //      String name, ModuleScope definitionScope, Type supertype, Type eigentype, boolean builtin)
  // {
  //    return new Type(name, definitionScope, supertype, eigentype, builtin);
  //  }

  //  public static Type createSingleton(String name, ModuleScope definitionScope, Type supertype,
  // Type eigentype, boolean builtin) {
  //
  //  }

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
}
