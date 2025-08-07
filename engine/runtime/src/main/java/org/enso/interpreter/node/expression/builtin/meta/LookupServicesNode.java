package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import java.util.ArrayList;
import org.enso.common.CompilationStage;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InteropApplicationNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.pkg.QualifiedName;
import org.enso.scala.wrapper.ScalaConversions;

@BuiltinMethod(
    type = "Meta",
    name = "lookup_services",
    description = "Looks services registered by a name up",
    autoRegister = false)
public abstract class LookupServicesNode extends Node {
  protected LookupServicesNode() {}

  public static LookupServicesNode build() {
    return new LookupServicesNode() {
      @Override
      protected Iterable<Type> findImplementationsFor(Type service) {
        return super.defaultImplementationsFor(service);
      }
    };
  }

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

  protected abstract Iterable<Type> findImplementationsFor(Type service);

  private final Iterable<Type> defaultImplementationsFor(Type fqn) {
    var found = new ArrayList<Type>();
    var ensoCtx = EnsoContext.get(this);
    for (var p : ensoCtx.getPackageRepository().getLoadedPackagesJava()) {
      var regs = ScalaConversions.asJava(p.getConfig().services());
      for (var pw : regs) {
        var spiTypeName = pw.provides();
        if (spiTypeName == null || !spiTypeName.equals(fqn.getQualifiedName())) {
          continue;
        }
        var implType = findType(pw.with(), ensoCtx);
        found.add(implType);
      }
    }
    return found;
  }

  @CompilerDirectives.TruffleBoundary
  public final EnsoObject execute(Type fqn) {
    var ensoCtx = EnsoContext.get(this);
    var collect = new ArrayList<TruffleObject>();
    for (var implType : findImplementationsFor(fqn)) {
      var conversion = UnresolvedConversion.build(implType.getDefinitionScope());
      var state = ensoCtx.currentState();
      var node = InteropApplicationNode.getUncached();
      var fn = conversion.resolveFor(ensoCtx, fqn, implType);
      if (fn == null) {
        var msg =
            "No conversion from "
                + implType.getQualifiedName()
                + " to "
                + fqn.getQualifiedName()
                + " found";
        collect.add(DataflowError.withDefaultTrace(Text.create(msg), this));
        continue;
      }
      var obj = node.execute(fn, state, new Object[] {fqn, implType});
      if (obj instanceof EnsoObject found) {
        collect.add(found);
      } else {
        throw ensoCtx.raiseAssertionPanic(this, "Expecting Enso object, but was: " + obj, null);
      }
    }
    var arr = collect.toArray(TruffleObject[]::new);
    return ArrayLikeHelpers.asVectorWithCheckAt((Object[]) arr);
  }
}
