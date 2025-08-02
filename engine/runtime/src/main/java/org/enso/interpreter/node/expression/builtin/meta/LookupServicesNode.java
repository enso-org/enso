package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import java.util.ArrayList;
import org.enso.common.CompilationStage;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InteropApplicationNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.pkg.QualifiedName;

@BuiltinMethod(
    type = "Meta",
    name = "lookup_services",
    description = "Looks services registered by a name up",
    autoRegister = false)
public final class LookupServicesNode extends Node {
  @CompilerDirectives.TruffleBoundary
  private Type findType(QualifiedName fqn, EnsoContext ensoCtx) {
    var module =
        switch (fqn.getParent().isDefined() ? 1 : 0) {
          case 1 -> {
            var moduleName = fqn.getParent().get();
            yield ensoCtx.getTopScope().getModule(moduleName.toString()).orElse(null);
          }
          default -> null;
        };
    if (module == null) {
      var err = ensoCtx.getBuiltins().error().makeModuleDoesNotExistError(fqn.toString());
      throw new PanicException(err, this);
    }
    var scope = module.compileScope(ensoCtx);
    var stage = module.getCompilationStage();
    assert stage.isAtLeast(CompilationStage.AFTER_CODEGEN) : "Unsufficient stage " + stage;

    var typeName = fqn.item();
    var implType = scope.getType(typeName, true);
    if (implType == null) {
      var sb = new StringBuilder();
      sb.append("Cannot find type ")
          .append(typeName)
          .append(" in ")
          .append(module.getName())
          .append(" module");
      var sep = ". Only found ";
      for (var typ : scope.getAllTypes()) {
        sb.append(sep);
        sb.append(typ.getName());
        sep = ", ";
      }
      var err = ensoCtx.getBuiltins().error().makeModuleDoesNotExistError(sb.toString());
      throw new PanicException(err, this);
    }
    return implType;
  }

  @CompilerDirectives.TruffleBoundary
  EnsoObject execute(Type fqn) {
    var ensoCtx = EnsoContext.get(this);
    var collect = new ArrayList<EnsoObject>();
    for (var p : ensoCtx.getPackageRepository().getLoadedPackagesJava()) {
      p.getConfig()
          .services()
          .foreach(
              pw -> {
                var spiTypeName = pw.provides();
                if (spiTypeName == null || !spiTypeName.equals(fqn.getQualifiedName())) {
                  return null;
                }

                var implType = findType(pw.with(), ensoCtx);
                var conversion = UnresolvedConversion.build(implType.getDefinitionScope());
                var state = ensoCtx.currentState();
                var node = InteropApplicationNode.getUncached();
                var fn = conversion.resolveFor(ensoCtx, fqn, implType);
                if (fn == null) {
                  var msg = "No conversion from " + implType.getName() + " to " + fqn.getQualifiedName() + " found";
                  throw ensoCtx.raiseAssertionPanic(this, msg, null);
                }
                var obj = node.execute(fn, state, new Object[] {fqn, implType});
                if (obj instanceof EnsoObject found) {
                  collect.add(found);
                } else {
                  throw ensoCtx.raiseAssertionPanic(this, "Expecting Enso object, but was: " + obj, null);
                }
                return null;
              });
    }
    var arr = collect.toArray(EnsoObject[]::new);
    return ArrayLikeHelpers.asVectorEnsoObjects(arr);
  }
}
