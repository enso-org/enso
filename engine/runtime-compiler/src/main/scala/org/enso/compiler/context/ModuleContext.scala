package org.enso.compiler.context

import org.enso.compiler.PackageRepository
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.pass.PassConfiguration
import org.enso.pkg.Package
import org.enso.pkg.QualifiedName
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.core.ir.Name

/** A type containing the information about the execution context for a module.
  *
  * @param module the current module scope
  * @param freshNameSupply the compiler's supply of fresh names
  * @param passConfiguration the pass configuration
  * @param compilerConfig the compiler configuration
  * @param isGeneratingDocs if true, should generate docs for IR
  * @param pkgRepo the compiler's package repository
  */
case class ModuleContext(
  module: CompilerContext.Module,
  compilerConfig: CompilerConfig,
  freshNameSupply: Option[FreshNameSupply]     = None,
  passConfiguration: Option[PassConfiguration] = None,
  isGeneratingDocs: Boolean                    = false,
  pkgRepo: Option[PackageRepository]           = None
) {
  def isSynthetic(): Boolean          = module.isSynthetic()
  def bindingsAnalysis(): BindingsMap = module.getBindingsMap()
  def getName(): QualifiedName        = module.getName()
  def getPackage(): Package[_]        = module.getPackage()
  def getCharacters(): CharSequence   = module.getCharacters()
  def moduleReference(): ModuleReference.Concrete =
    ModuleReference.Concrete(module)
  val anyIr = Name.Qualified
    .builder()
    .parts(
      List(
        lit("Standard"),
        lit("Base"),
        lit("Any"),
        lit("Any")
      )
    )
    .build()

  private def lit(name: String): Name.Literal = {
    Name.Literal.builder().name(name).isMethod(false).build()
  }

}
