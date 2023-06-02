package org.enso.compiler.pass.analyse

import org.enso.interpreter.runtime.Module
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}
import org.enso.compiler.pass.resolve.{MethodDefinitions, Patterns}

import scala.annotation.unused

/** Recognizes all defined bindings in the current module and constructs
  * a mapping data structure that can later be used for symbol resolution.
  */
case object ImportApiAnalysis extends IRPass {

  override type Metadata = BindingsMap

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(ComplexType, FunctionBinding, GenerateMethodBodies)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] =
    Seq(MethodDefinitions, Patterns)

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir            the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val map: BindingsMap =
      ir.unsafeGetMetadata(BindingAnalysis, "Should exist.")
    val forbiddenImports = new java.util.HashMap[String, String]

    map.resolvedImports.map { imp: BindingsMap.ResolvedImport =>
      val maybeModule = imp.target match {
        case rm: BindingsMap.ResolvedModule =>
          None
          rm.module match {
            case c: BindingsMap.ModuleReference.Concrete => Some(c.module)
            case _                                       => None
          }
        case _ => None
      }

      maybeModule.map(m => {
        val pkg = m.getPackage()
        if (moduleContext.module.getPackage() != pkg) {
          if (pkg.mainFile != null) {
            val found = pkg.findModule("Main")
            if (found != null) {
              // if the package has Main file, then imports has to go thru the main module
              val mainModule = found.asInstanceOf[Module]
              if (m != mainModule) {
                // if different that Main module is requested, do a check
                if (mainModule.getIr == null) {
                  // if main module IR isn't loaded, then certainly the import didn't go thru Main
                  forbiddenImports.put(
                    m.getName().toString(),
                    mainModule.getName().toString()
                  )
                } else {
                  val mainMap: BindingsMap = mainModule.getIr
                    .unsafeGetMetadata(BindingAnalysis, "Should exist.")
                  val checks = for {
                    // go thru all re-exported modules in main
                    (name, modules) <- mainMap.exportedSymbols
                    module          <- modules
                  } yield {
                    module match {
                      case BindingsMap.ResolvedModule(
                            BindingsMap.ModuleReference.Concrete(allowed)
                          ) => {
                        // check if one of the exported modules matches the imported one
                        allowed == m
                      }
                      case _ => false
                    }
                  }
                  if (!checks.exists(b => b)) {
                    // if no re-exported module patches m, report an error
                    forbiddenImports.put(
                      m.getName().toString(),
                      mainModule.getName().toString()
                    )
                  }
                }
              }
            }
          }
        }
      })
    }
    val replace = ir.imports.map(imp =>
      imp match {
        case m: IR.Module.Scope.Import.Module =>
          val name: String = m.name.name
          val pkg          = forbiddenImports.get(name)
          if (pkg != null) {
            IR.Error.ImportExport(
              imp,
              IR.Error.ImportExport
                .SymbolDoesNotExist(name.split("\\.").last, pkg)
            )
          } else {
            m
          }
        case i => i
      }
    )
    ir.copy(imports = replace)
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
