package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.context.CompilerContext;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class CacheClosingTest {

  @Rule public final TemporaryFolder tmpFolder = new TemporaryFolder();

  @Test
  public void cacheCannotBeSavedTwice() throws IOException {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject("Proj", """
        main =
            42
        """, projDir);
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(bldr -> bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false"))
            .withProjectRoot(projDir)
            .build()) {
      var polyCtx = new PolyglotContext(ctx.context());
      polyCtx.getTopScope().compile(true);
      var compilerCtx = ctx.ensoContext().getCompiler().context();
      var modOpt = ctx.ensoContext().getPackageRepository().getLoadedModule("local.Proj.Main");
      assertThat(modOpt.isDefined(), is(true));
      var mod = modOpt.get();
      boolean serialized = false;
      try {
        serialized = serialize(compilerCtx, ctx, mod);
      } catch (ExecutionException | InterruptedException e) {
        fail("First serialization should be OK");
      }
      assertThat("First serialization should be OK", serialized, is(true));

      try {
        serialize(compilerCtx, ctx, mod);
        fail("Second serialization should fail");
      } catch (ExecutionException | InterruptedException e) {
        // OK
      }
    }
  }

  private static boolean serialize(
      CompilerContext compilerCtx, ContextUtils ctx, CompilerContext.Module mod)
      throws ExecutionException, InterruptedException {
    var serializeFut =
        compilerCtx.serializeModule(ctx.ensoContext().getCompiler(), mod, false, false);
    return serializeFut.get();
  }
}
