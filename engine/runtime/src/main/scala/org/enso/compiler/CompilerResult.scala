package org.enso.compiler

import org.enso.interpreter.runtime.Module

import scala.jdk.CollectionConverters._

/** The result of running the compiler.
  *
  * @param compiledModules the modules compiled during the run
  */
final case class CompilerResult(compiledModules: CompiledModules)
object CompilerResult {

  /** @return an empty compiler result */
  def empty: CompilerResult =
    CompilerResult(CompiledModules(Nil))
}

/** The list of compiled modules.
  *
  * @param modules the list of compiled modules
  */
final case class CompiledModules(modules: List[Module]) {

  /** Convert the modules list to a Java collection. */
  def asJava: java.util.List[Module] =
    modules.asJava

  /** Combine two [[CompiledModules]].
    *
    * @param other the other compiled modules
    * @return the new [[CompiledModules]] instance holding both lists of modules
    */
  def append(other: CompiledModules): CompiledModules =
    copy(modules = modules ++ other.modules)
}
