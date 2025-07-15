package org.enso.compiler;

import org.enso.compiler.context.CompilerContext.Module;
import org.enso.scala.wrapper.ScalaConversions;
import scala.collection.immutable.List;

/**
 * The result of running the compiler.
 *
 * @param compiledModules the modules compiled during the run
 */
public record CompilerResult(List<Module> compiledModules) {
  /**
   * @return an empty compiler result
   */
  public static CompilerResult empty() {
    return new CompilerResult(ScalaConversions.nil());
  }
}
