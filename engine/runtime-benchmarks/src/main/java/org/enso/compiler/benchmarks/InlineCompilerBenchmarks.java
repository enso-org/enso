package org.enso.compiler.benchmarks;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.compiler.context.InlineContext;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.MethodNames.TopScope;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Measures the inline compilation, that is the compilation that is requested inside a method.
 * Simulates a scenario where there is an existing method and we are trying to insert a new
 * expression into it.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(0)
@Warmup(iterations = 1)
@Measurement(iterations = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class InlineCompilerBenchmarks {
  @Setup
  public void setup() throws IOException {
    var ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .logHandler(System.err)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    var ensoCtx = ctx
        .getBindings(LanguageInfo.ID)
        .invokeMember(TopScope.LEAK_CONTEXT)
        .as(EnsoContext.class);
    var code = """
      main = 1 + 1
      """;
    var srcFile = createSrcFile(code, "bench-1.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = ctx.eval(src);
    var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var assocTypeReceiver = (Type) Utils.unwrapReceiver(ctx, assocType);
    var moduleScope = assocTypeReceiver.getDefinitionScope();
    var mainFunc = moduleScope.getMethodForType(assocTypeReceiver, "main");
    var mainFuncRootNode = (MethodRootNode) mainFunc.getCallTarget().getRootNode();
    var localScope = mainFuncRootNode.getLocalScope();
    var compiler = ensoCtx.getCompiler();
    InlineContext inlineContext =
        InlineContext.fromJava(
            localScope,
            moduleScope.getModule().asCompilerModule(),
            scala.Option.apply(false),
            ensoCtx.getCompilerConfig(),
            scala.Option.apply(compiler.packageRepository()));
    var inlineExpr = "42 * 2";
    var tuppleOpt = compiler.runInline(inlineExpr, inlineContext);
    assert tuppleOpt.isDefined();
    var newInlineContext = tuppleOpt.get()._1();
    var ir = tuppleOpt.get()._2();
    var newSrc = tuppleOpt.get()._3();

    //
    System.out.println("Done bench init");
  }

  private static File createSrcFile(String code, String name) {
    var benchDataDir = Path.of(".", "target", "bench-data");
    var srcFile = benchDataDir.resolve(name).toFile();
    try {
      Files.writeString(srcFile.toPath(), code);
    } catch (IOException e) {
      throw new AssertionError(e);
    }
    return srcFile;
  }
}
