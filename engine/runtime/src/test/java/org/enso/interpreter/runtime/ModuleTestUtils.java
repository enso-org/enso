package org.enso.interpreter.runtime;

import org.enso.polyglot.CompilationStage;

public final class ModuleTestUtils {
  private ModuleTestUtils() {}

  public static void unsafeSetIr(Module m, org.enso.compiler.core.ir.Module ir) {
    m.unsafeSetIr(ir);
  }

  public static void unsafeSetCompilationStage(Module m, CompilationStage s) {
    m.unsafeSetCompilationStage(s);
  }
}
