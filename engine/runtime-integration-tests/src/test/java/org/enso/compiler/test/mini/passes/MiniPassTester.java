package org.enso.compiler.test.mini.passes;

import java.io.IOException;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.PassConfiguration;
import org.enso.compiler.test.CompilerTests;
import org.enso.pkg.QualifiedName;
import scala.Option;

/**
 * A tester class that asserts that a {@link MiniIRPass} has the same result as its corresponding
 * {@link IRPass} in a module compilation.
 */
public abstract class MiniPassTester {
  protected void compareModuleCompilation(
      Module ir, ModuleContext moduleContext, IRPass megaPass, MiniPassFactory miniPassFactory) {
    var miniPass = miniPassFactory.createForModuleCompilation(moduleContext);
    if (miniPass == null) {
      throw new IllegalArgumentException("Mini pass does not support module compilation");
    }
    var megaPassResult = megaPass.runModule(ir, moduleContext);
    var miniPassResult = MiniIRPass.compile(Module.class, ir, miniPass);
    try {
      CompilerTests.assertIR(
          "Mini pass and mega pass results are equal", megaPassResult, miniPassResult);
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  protected Module parse(String code) {
    var modIr = EnsoParser.compile(code);
    return modIr;
  }

  protected ModuleContext buildModuleContext(QualifiedName moduleName) {
    var compilerConf = defaultCompilerConfig();
    Option<FreshNameSupply> freshNameSupply = Option.empty();
    Option<PassConfiguration> passConfig = Option.empty();
    Option<PackageRepository> pkgRepo = Option.empty();
    var isGeneratingDocs = false;
    var runtimeMod = org.enso.interpreter.runtime.Module.empty(moduleName, null);
    return ModuleContext.apply(
        runtimeMod.asCompilerModule(),
        compilerConf,
        freshNameSupply,
        passConfig,
        isGeneratingDocs,
        pkgRepo);
  }

  private static CompilerConfig defaultCompilerConfig() {
    return CompilerConfig.apply(false, true, true, false, false, false, false, Option.empty());
  }
}
