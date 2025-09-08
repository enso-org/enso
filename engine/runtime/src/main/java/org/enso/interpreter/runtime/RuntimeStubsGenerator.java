package org.enso.interpreter.runtime;

import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.scala.wrapper.ScalaConversions;

/**
 * Generates stubs of runtime representations of atom constructors, to allow [[IrToTruffle the code
 * generator]] to refer to constructors that are not fully generated yet.
 */
final class RuntimeStubsGenerator {
  private final Builtins builtins;

  RuntimeStubsGenerator(Builtins builtins) {
    this.builtins = builtins;
  }

  /**
   * Runs the stage on the given module.
   *
   * @param module the module to generate stubs in.
   */
  void run(IR ir, TruffleCompilerModuleScopeBuilder scope) {
    var localBindings = (BindingsMap) ir.passData().get(BindingAnalysis$.MODULE$).get();
    var types =
        localBindings
            .definedEntities()
            .filter(t -> t instanceof BindingsMap.Type)
            .map(t -> (BindingsMap.Type) t);
    var compilerScope = scope.toCompilerBuilder();
    types.foreach(
        tp -> {
          if (tp.builtinType()) {
            var builtinType = builtins.getBuiltinType(tp.name());
            if (builtinType == null) {
              throw new CompilerError("Unknown @Builtin_Type " + tp.name());
            }
            var tpNames = tp.members().map(c -> c.name()).toSet();
            var exNames =
                ScalaConversions.set(builtinType.getConstructors())
                    .map(AtomConstructor::getName)
                    .toSet();
            if (!tpNames.equals(exNames)) {
              throw new CompilerError(
                  "Wrong constructors declared in the builtin " + tp.name() + ".");
            }
            scope.registerType(builtinType.getType());
            builtinType.getType().setShadowDefinitions(builtins.getLanguage(), compilerScope, true);
          } else {
            var hasAllConstructorsPrivate =
                tp.isPrivate()
                    || tp.members().nonEmpty() && tp.members().forall(c -> c.isProjectPrivate());
            var createdType =
                (tp.members().nonEmpty() || tp.builtinType())
                    ? Type.create(
                        builtins.getLanguage(),
                        tp.name(),
                        compilerScope,
                        builtins.any(),
                        builtins.any(),
                        false,
                        hasAllConstructorsPrivate)
                    : Type.createSingleton(
                        tp.name(), compilerScope, builtins.any(), false, hasAllConstructorsPrivate);
            var rtp = scope.registerType(createdType);
            tp.members()
                .foreach(
                    cons -> {
                      var constructor =
                          new AtomConstructor(cons.name(), scope.getModule(), rtp, false);
                      rtp.registerConstructor(constructor);
                      return null;
                    });
          }
          return null;
        });
  }
}
