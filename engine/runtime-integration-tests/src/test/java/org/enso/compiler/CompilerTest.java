package org.enso.compiler;

import static org.junit.Assert.assertTrue;

import java.util.List;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.pass.PassConfiguration;
import org.enso.compiler.pass.PassManager;
import org.enso.compiler.test.CompilerRunner;
import org.enso.pkg.QualifiedName;
import org.graalvm.polyglot.Source;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;

public abstract class CompilerTest extends ParserTest {
  /**
   * Note that this `compile` method will not run import resolution. For now we just have tests that
   * do not need it, and tests that do need it are placed in {@link
   * org.enso.interpreter.test.TypeInferenceConsistencyTest} which spawns the whole interpreter.
   *
   * <p>If we want to run the imports resolution here, we need to create an instance of {@link
   * Compiler}, like in {@link org.enso.compiler.test.semantic.TypeSignaturesTest}, but that relies
   * on spawning a Graal context anyway. If possible I think it's good to skip that so that these
   * tests can be kept simple - and the more complex ones can be done in the other suite.
   */
  protected Module compile(Source src) {
    if (src.getCharacters().toString().contains("import")) {
      throw new IllegalArgumentException("This method will not work correctly with imports.");
    }

    Module rawModule = parse(src.getCharacters());

    var compilerConfig = new CompilerConfig(false, true, true, true, true, Option.empty());
    var passes = new Passes(compilerConfig, Option.empty());
    @SuppressWarnings("unchecked")
    var passConfig =
        new PassConfiguration((Seq<PassConfiguration.ConfigPair<?>>) Seq$.MODULE$.empty());
    PassManager passManager = new PassManager(passes.passOrdering(), passConfig);
    var compilerRunner =
        new CompilerRunner() {
          @Override
          public CompilerConfig defaultConfig() {
            return compilerConfig;
          }

          @Override
          public void org$enso$compiler$test$CompilerRunner$_setter_$defaultConfig_$eq(
              CompilerConfig x$1) {}
        };
    var moduleName = QualifiedName.simpleName(src.getName().replace(".enso", ""));
    ModuleContext moduleContext =
        compilerRunner.buildModuleContext(
            moduleName, Option.apply(new FreshNameSupply()), Option.empty(), compilerConfig, false);
    Module processedModule = passManager.runPassesOnModule(rawModule, moduleContext);
    return processedModule;
  }

  protected final List<Diagnostic> getImmediateDiagnostics(IR ir) {
    return CollectionConverters.asJava(ir.diagnostics().toList());
  }

  protected final List<Diagnostic> getDescendantsDiagnostics(IR ir) {
    return CollectionConverters.asJava(
        ir.preorder().flatMap((node) -> node.diagnostics().toList()));
  }

  protected final Method findStaticMethod(Module module, String name) {
    var option =
        module
            .bindings()
            .find(
                (def) ->
                    (def instanceof Method binding)
                        && binding.methodReference().typePointer().isEmpty()
                        && binding.methodReference().methodName().name().equals(name));

    assertTrue("The method " + name + " should exist within the IR.", option.isDefined());
    return (Method) option.get();
  }

  protected final Method findMemberMethod(Module module, String typeName, String name) {
    var option =
        module
            .bindings()
            .find(
                (def) ->
                    (def instanceof Method binding)
                        && binding.methodReference().typePointer().isDefined()
                        && binding.methodReference().typePointer().get().name().equals(typeName)
                        && binding.methodReference().methodName().name().equals(name));

    assertTrue("The method " + name + " should exist within the IR.", option.isDefined());
    return (Method) option.get();
  }

  protected final Expression.Binding findAssignment(IR ir, String name) {
    var option =
        ir.preorder()
            .find(
                (node) ->
                    (node instanceof Expression.Binding binding)
                        && binding.name().name().equals(name));
    assertTrue("The binding `" + name + " = ...` should exist within the IR.", option.isDefined());
    return (Expression.Binding) option.get();
  }
}
