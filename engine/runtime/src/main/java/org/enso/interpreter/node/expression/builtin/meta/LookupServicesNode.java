package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import java.util.ArrayList;
import java.util.function.Supplier;
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
  /** Protected for mocking purposes */
  protected LookupServicesNode() {}

  public static LookupServicesNode build() {
    return new LookupServicesNode() {
      @Override
      protected Iterable<Supplier<Type>> findImplementationsFor(Type service) {
        return super.defaultImplementationsFor(service);
      }
    };
  }

  @CompilerDirectives.TruffleBoundary
  private final Type findType(QualifiedName fqn, String namespace, String name) {
    var ensoCtx = EnsoContext.get(this);

    var path = fqn.pathAsJava();
    if (path.size() < 2 || !namespace.equals(path.get(0)) || !name.equals(path.get(1))) {
      var err = ensoCtx.getBuiltins().error().makeModuleNotInPackageError();
      throw new PanicException(err, this);
    }

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
    assert !module.needsCompilation() : "Unsufficient stage of " + module;

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

  /**
   * Implements the lookup of all type registrations for given {@code service} type. This method is
   * protected to allow unit testing with mocks. The expected implementation is supposed to find all
   * possible registrations and return them as {@link Iterable}. Each element of the iterable is
   * "supplier" that can either return the Type implementing the service or yield {@link
   * AbstractTruffleException} (like {@link PanicException}) to signal that the registration is
   * there, but broken.
   *
   * @param service the type to find implementations for
   * @return iterable with suppliers of types
   */
  protected abstract Iterable<Supplier<Type>> findImplementationsFor(Type service);

  private final Iterable<Supplier<Type>> defaultImplementationsFor(Type fqn) {
    var found = new ArrayList<Supplier<Type>>();
    var ensoCtx = EnsoContext.get(this);
    for (var p : ensoCtx.getPackageRepository().getLoadedPackagesJava()) {
      var regs = ScalaConversions.asJava(p.getConfig().services());
      for (var pw : regs) {
        var spiTypeName = pw.provides();
        if (spiTypeName == null || !spiTypeName.equals(fqn.getQualifiedName())) {
          continue;
        }
        var with = pw.with();
        found.add(() -> findType(with, p.namespace(), p.name()));
      }
    }
    return found;
  }

  @CompilerDirectives.TruffleBoundary
  public final EnsoObject execute(Type fqn) {
    var ensoCtx = EnsoContext.get(this);
    var collect = new ArrayList<Object>();
    for (var supplierOfType : findImplementationsFor(fqn)) {
      try {
        // the get() call may yield AbstractTruffleException
        var implType = supplierOfType.get();
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
        collect.add(obj);
      } catch (AbstractTruffleException ex) {
        collect.add(DataflowError.withDefaultTrace(Text.create(ex.getMessage()), this));
      }
    }
    var arr = collect.toArray(TruffleObject[]::new);
    return ArrayLikeHelpers.asVectorWithCheckAt((Object[]) arr);
  }
}
