package org.enso.compiler.test;

import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.test.InterpreterContext;
import org.enso.pkg.QualifiedName;
import org.graalvm.polyglot.Source;
import scala.jdk.javaapi.CollectionConverters;

public abstract class StaticAnalysisTest {

  /**
   * The interpreter context is needed here as it ensures initialization of everything needed to
   * perform imports resolution, including PackageRepository.
   *
   * <p>Ideally, the tests for the static analysis capabilities of the compiler should _not_ depend
   * on the Graal runtime context, as they should be runnable in other contexts - i.e. in a Visual
   * Studio Code language server.
   */
  private final InterpreterContext interpreterContext =
      new InterpreterContext(
          (builder) ->
              builder
                  .option(RuntimeOptions.ENABLE_STATIC_ANALYSIS, "true")
                  .option(RuntimeOptions.LOG_LEVEL, Level.INFO.getName())
          // TODO we want to disable stdout of the tests - but for now it's useful for debugging
          //                  .option(RuntimeOptions.LOG_LEVEL, Level.SEVERE.getName())
          //                  .out(OutputStream.nullOutputStream())
          //                  .err(OutputStream.nullOutputStream())
          );

  private final EnsoContext langCtx =
      interpreterContext
          .ctx()
          .getBindings(LanguageInfo.ID)
          .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
          .asHostObject();

  protected Module compile(Source src) {
    String suffix = ".enso";
    String name = src.getName();
    if (!name.endsWith(suffix)) {
      throw new IllegalArgumentException("Source name must end with " + suffix);
    }
    QualifiedName qualifiedName =
        QualifiedName.fromString(name.substring(0, name.length() - suffix.length()));

    // If the module name is supposed to be put in a project, we register a synthetic project entry
    // for it
    if (qualifiedName.path().length() >= 2) {
      LibraryName libraryName =
          new LibraryName(qualifiedName.path().apply(0), qualifiedName.path().apply(1));
      if (!langCtx.getPackageRepository().isPackageLoaded(libraryName)) {
        langCtx
            .getPackageRepository()
            .registerSyntheticPackage(libraryName.namespace(), libraryName.name());
        assert langCtx.getPackageRepository().isPackageLoaded(libraryName);
      }
    }

    // This creates the module and also registers it in the scope, so that import resolution will
    // see it.
    var module =
        langCtx.getTopScope().createModule(qualifiedName, null, src.getCharacters().toString());
    langCtx.getCompiler().run(module.asCompilerModule());
    return module.getIr();
  }

  protected final List<Diagnostic> getImmediateDiagnostics(IR ir) {
    return CollectionConverters.asJava(ir.getDiagnostics().toList());
  }

  protected final List<Diagnostic> getDescendantsDiagnostics(IR ir) {
    return CollectionConverters.asJava(
        ir.preorder()
            .flatMap(
                (node) -> {
                  DiagnosticStorage diagnostics = node.getDiagnostics();
                  if (diagnostics != null) {
                    return diagnostics.toList();
                  } else {
                    return scala.collection.immutable.List$.MODULE$.empty();
                  }
                }));
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
