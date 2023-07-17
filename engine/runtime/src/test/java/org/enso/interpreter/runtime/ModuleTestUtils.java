package org.enso.interpreter.runtime;

import org.enso.compiler.core.IR;
import org.enso.polyglot.CompilationStage;

public final class ModuleTestUtils {
  private ModuleTestUtils() {}

  public static void unsafeSetIr(Module m, IR.Module ir) {
    m.unsafeSetIr(ir);
  }

  public static void unsafeSetCompilationStage(Module m, CompilationStage s) {
    m.unsafeSetCompilationStage(s);
  }
}
