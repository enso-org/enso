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

    var scopeBuilder =
        new StaticModuleScope.Builder(QualifiedName.fromString("Standard.Builtins.Main"));
    scopeBuilder.registerMethod(TypeScopeReference.ANY, "to_text", builtinTypes.TEXT);
    scopeBuilder.registerMethod(TypeScopeReference.ANY, "to_display_text", builtinTypes.TEXT);
    scopeBuilder.registerMethod(TypeScopeReference.ANY, "pretty", builtinTypes.TEXT);

    var any = new TypeRepresentation.TopType();
    scopeBuilder.registerMethod(
        TypeScopeReference.ANY, "==", new TypeRepresentation.ArrowType(any, builtinTypes.BOOLEAN));

    var catchType =
        new TypeRepresentation.ArrowType(new TypeRepresentation.ArrowType(any, any), any);
    scopeBuilder.registerMethod(TypeScopeReference.ANY, "catch_primitive", catchType);

    cachedAnyScope = scopeBuilder.build();
    return cachedAnyScope;
  }
}
