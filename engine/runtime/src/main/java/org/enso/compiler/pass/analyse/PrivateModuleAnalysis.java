package org.enso.compiler.pass.analyse;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.pass.IRPass;
import org.enso.interpreter.util.ScalaConversions;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

public class PrivateModuleAnalysis implements IRPass {
  public static final PrivateModuleAnalysis MODULE$ = new PrivateModuleAnalysis();
  private UUID uuid;

  private PrivateModuleAnalysis() {}

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    List<IRPass> passes = List.of(
        BindingAnalysis$.MODULE$,
        ImportSymbolAnalysis$.MODULE$
    );
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRPass> invalidatedPasses() {
    return ScalaConversions.nil();
  }

  @Override
  public Module runModule(Module moduleIr, ModuleContext moduleContext) {
    var bindingsMap = moduleContext.bindingsAnalysis();
    var currentPackage = moduleContext.getPackage();
    List<Import> errors = new ArrayList<>();
    // Check if imported modules are not private
    bindingsMap.resolvedImports().foreach(resolvedImp -> {
      var importedModule = resolvedImp.target().module().unsafeAsModule("should succeed");
      var importedModuleName = importedModule.getName().toString();
      var importedModulePackage = importedModule.getPackage();
      if (currentPackage != null
          && !currentPackage.equals(importedModulePackage)
          && importedModule.isPrivate()) {
        errors.add(ImportExport.apply(
            resolvedImp.importDef(),
            new ImportExport.ImportPrivateModule(importedModuleName),
            ImportExport.apply$default$3(),
            ImportExport.apply$default$4()
        ));
      }
      return null;
    });

    // Check if we try to export anything from a private module
    if (moduleIr.isPrivate() && !moduleIr.exports().isEmpty()) {
      errors.add(ImportExport.apply(
          moduleIr.exports().apply(0),
          new ImportExport.ExportPrivateModule(moduleContext.getName().toString()),
          ImportExport.apply$default$3(),
          ImportExport.apply$default$4()
      ));
    }

    scala.collection.immutable.List<Import> convertedImports =
        errors.isEmpty() ? moduleIr.imports() : CollectionConverters.asScala(errors).toList();

    return moduleIr.copy(
        convertedImports,
        moduleIr.exports(),
        moduleIr.bindings(),
        moduleIr.location(),
        moduleIr.passData(),
        moduleIr.diagnostics(),
        moduleIr.id()
    );
  }

  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }
}
