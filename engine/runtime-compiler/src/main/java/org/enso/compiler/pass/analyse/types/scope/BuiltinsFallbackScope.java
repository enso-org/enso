package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.pass.analyse.types.BuiltinTypes;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;

public class BuiltinsFallbackScope {
  private final BuiltinTypes builtinTypes;
  private StaticModuleScope cachedAnyScope = null;

  public BuiltinsFallbackScope(BuiltinTypes builtinTypes) {
    this.builtinTypes = builtinTypes;
  }

  public StaticModuleScope fallbackAnyScope() {
    if (cachedAnyScope != null) {
      return cachedAnyScope;
    }

    var scope = new StaticModuleScope(QualifiedName.fromString("Standard.Builtins.Main"));
    scope.registerMethod(TypeScopeReference.ANY, "to_text", builtinTypes.TEXT);
    scope.registerMethod(TypeScopeReference.ANY, "to_display_text", builtinTypes.TEXT);
    scope.registerMethod(TypeScopeReference.ANY, "pretty", builtinTypes.TEXT);

    var any = new TypeRepresentation.TopType();
    scope.registerMethod(
        TypeScopeReference.ANY, "==", new TypeRepresentation.ArrowType(any, builtinTypes.BOOLEAN));

    var catchType =
        new TypeRepresentation.ArrowType(new TypeRepresentation.ArrowType(any, any), any);
    scope.registerMethod(TypeScopeReference.ANY, "catch_primitive", catchType);

    cachedAnyScope = scope;
    return cachedAnyScope;
  }
}
