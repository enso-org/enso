package org.enso.compiler;

import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.function.Function;

import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
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
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.test.InterpreterContext;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;
import scala.jdk.javaapi.CollectionConverters;

public abstract class StaticAnalysisTest {

  /**
   * The interpreter context is needed here as it ensures initialization of everything needed to perform imports
   * resolution, including PackageRepository.
   * <p>
   * Ideally, the tests for the static analysis capabilities of the compiler should _not_ depend on the Graal runtime
   * context, as they should be runnable in other contexts - i.e. in a Visual Studio Code language server.
   */
  private final InterpreterContext interpreterContext = new InterpreterContext(
      (builder) -> builder.option(RuntimeOptions.ENABLE_STATIC_ANALYSIS, "true")
  );
  private final EnsoContext langCtx = interpreterContext.ctx()
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject();

  protected Module compile(Source src) {
    String suffix = ".enso";
    String name = src.getName();
    if (!name.endsWith(suffix)) {
      throw new IllegalArgumentException("Source name must end with " + suffix);
    }
    QualifiedName qualifiedName = QualifiedName.fromString(name.substring(0, name.length() - suffix.length()));
    var module = new org.enso.interpreter.runtime.Module(qualifiedName, null, src.getCharacters().toString());
    langCtx.getCompiler().run(module.asCompilerModule());
    return module.getIr();
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
