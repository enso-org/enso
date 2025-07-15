package org.enso.compiler.pass.desugar;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.scala.wrapper.ScalaConversions;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;

public final class Imports implements MiniPassFactory {
  public static final Imports INSTANCE = new Imports();
  public static final Name.Literal mainModuleName = name("Main");
  private static final String currentProjectAlias = "project";

  private Imports() {}

  private static Name.Literal name(String name) {
    return new Name.Literal(name, false, null, Option.empty(), new MetadataStorage());
  }

  private static Name.Qualified withNewParts(Name.Qualified name, List<Name> newParts) {
    return name.copy(
        newParts,
        name.copy$default$2(),
        name.copy$default$3(),
        name.copy$default$4(),
        name.copy$default$5());
  }

  private Option<Name.Qualified> desugarCurrentProjectAlias(
      Name.Qualified name, ModuleContext context) {
    var parts = name.parts();
    if (parts.size() >= 1 && parts.apply(0).name().equals(currentProjectAlias)) {
      var pkgOpt = Option.apply(context.getPackage());
      return pkgOpt.map(
          pkg -> {
            var namespace = name(pkg.namespace());
            var pkgName = name(pkg.normalizedName());
            @SuppressWarnings("unchecked")
            var newParts =
                ScalaConversions.cons(
                    namespace, ScalaConversions.cons(pkgName, (List<Name>) parts.tail()));
            return withNewParts(name, newParts);
          });
    } else {
      return Option.apply(name);
    }
  }

  private Option<Name.Literal> computeRename(
      Option<Name.Literal> originalRename, Boolean onlyNames, Name.Literal qualName) {
    if (originalRename.isDefined()) {
      return originalRename;
    } else if (onlyNames) {
      return Option.empty();
    } else {
      return Option.apply(qualName);
    }
  }

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    return ScalaConversions.nil();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    return ScalaConversions.nil();
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return new Mini(moduleContext);
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  private final class Mini extends MiniIRPass {
    private final ModuleContext moduleContext;

    private Mini(ModuleContext moduleContext) {
      this.moduleContext = moduleContext;
    }

    @Override
    public Expression transformExpression(Expression expr) {
      return null;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      var newImports =
          moduleIr
              .imports()
              .map(
                  imp ->
                      switch (imp) {
                        case Import.Module i -> {
                          var fqn = desugarCurrentProjectAlias(i.name(), moduleContext);
                          Option<Import> opt =
                              fqn.map(
                                  (name) -> {
                                    var parts = name.parts();
                                    if (parts.length() == 2) {
                                      @SuppressWarnings("unchecked")
                                      var withMain =
                                          withNewParts(
                                              name, (List<Name>) parts.appended(mainModuleName));
                                      var withRename =
                                          computeRename(
                                              i.rename(),
                                              i.onlyNames().nonEmpty() || i.isAll(),
                                              (Name.Literal) parts.apply(1));
                                      return i.copyWithNameAndRename(withMain, withRename);
                                    } else {
                                      return i.copyWithName(name);
                                    }
                                  });
                          if (opt.isDefined()) {
                            yield opt.get();
                          } else {
                            var err =
                                new ImportExport(
                                    i,
                                    new ImportExport.ProjectKeywordUsedButNotInProject("import"),
                                    new MetadataStorage());
                            yield err;
                          }
                        }
                        default -> imp;
                      });

      var newExports =
          moduleIr
              .exports()
              .map(
                  exp ->
                      switch (exp) {
                        case Export.Module ex -> {
                          var fqn = desugarCurrentProjectAlias(ex.name(), moduleContext);
                          var opt =
                              fqn.map(
                                  (name) -> {
                                    var parts = name.parts();
                                    if (parts.length() == 2) {
                                      @SuppressWarnings("unchecked")
                                      var withMain =
                                          withNewParts(
                                              name, (List<Name>) parts.appended(mainModuleName));
                                      var withRename =
                                          computeRename(
                                              ex.rename(),
                                              ex.onlyNames().nonEmpty(),
                                              (Name.Literal) parts.apply(1));
                                      return exportWithNameAndRename(ex, withMain, withRename);
                                    } else {
                                      return exportWithNameAndRename(ex, name, null);
                                    }
                                  });
                          if (opt.isDefined()) {
                            yield opt.get();
                          } else {
                            var err =
                                new ImportExport(
                                    ex,
                                    new ImportExport.ProjectKeywordUsedButNotInProject("export"),
                                    new MetadataStorage());
                            yield err;
                          }
                        }
                        default -> exp;
                      });
      return moduleIr.copyWithImportsAndExports(newImports, newExports);
    }

    private Export.Module exportWithNameAndRename(
        Export.Module ex, Name.Qualified withMain, Option<Name.Literal> withRename) {
      if (withRename == null) {
        withRename = ex.copy$default$2();
      }
      return ex.copy(
          withMain,
          withRename,
          ex.copy$default$3(),
          ex.copy$default$4(),
          ex.copy$default$5(),
          ex.copy$default$6(),
          ex.copy$default$7(),
          ex.copy$default$8());
    }
  }
}
