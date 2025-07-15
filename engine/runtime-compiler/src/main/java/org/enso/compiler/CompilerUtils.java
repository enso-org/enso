package org.enso.compiler;

import java.util.ArrayList;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.scala.wrapper.ScalaConversions;
import scala.collection.immutable.List;

final class CompilerUtils {
  private CompilerUtils() {}

  /**
   * Run the compiler on the list of modules.
   *
   * <p>The compilation may load the libraries defining component groups. To ensure that the symbols
   * defined by the component groups are also compiled, this method is called recursively.
   */
  static CompilerResult runInternal(
      Compiler compiler,
      List<Module> modulesToCompile,
      boolean generateCode,
      boolean shouldCompileDependencies,
      boolean generateDocs) {
    var processed = new ArrayList<Module>();
    while (true) {
      if (modulesToCompile.isEmpty()) {
        return new CompilerResult(ScalaConversions.asScala(processed));
      }
      var newCompiled =
          compiler.runCompilerPipeline(
              modulesToCompile, generateCode, shouldCompileDependencies, generateDocs);
      var pending = compiler.packageRepository().getPendingModules().toList();

      processed.addAll(ScalaConversions.asJava(newCompiled));
      modulesToCompile = pending;
    }
  }
}
