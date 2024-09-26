package org.enso.compiler.test.mini.passes;

import java.io.IOException;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.MiniPassTraverser;
import org.enso.compiler.test.CompilerTests;

/**
 * A tester class that asserts that a {@link MiniIRPass} has the same result as its corresponding
 * {@link IRPass} in a module compilation.
 */
public class MiniPassTester {
  public void compareModuleCompilation(
      Module ir,
      ModuleContext moduleContext,
      IRPass megaPass,
      MiniPassFactory<? extends MiniIRPass> miniPassFactory) {
    var miniPass = miniPassFactory.createForModuleCompilation(moduleContext);
    if (miniPass == null) {
      throw new IllegalArgumentException("Mini pass does not support module compilation");
    }
    var megaPassResult = megaPass.runModule(ir, moduleContext);
    var miniPassResult = MiniPassTraverser.compileWithMiniPass(ir, miniPass);
    try {
      CompilerTests.assertIR(
          "Mini pass and mega pass results are equal", megaPassResult, miniPassResult);
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }
}
