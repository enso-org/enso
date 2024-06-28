package org.enso.compiler.phase;

import java.io.IOException;
import java.util.Objects;

import java.util.stream.Collectors;
import org.enso.common.CompilationStage;
import org.enso.compiler.Compiler;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.ImportExport;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedImport;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.editions.LibraryName;

import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.jdk.CollectionConverters;

abstract class ImportResolverForIR extends ImportResolverAlgorithm<
  Tuple2<Import, Option<BindingsMap.ResolvedImport>>,
  Module,
  Import.Module,
  Export.Module,
  ResolvedType,
  CompilerContext.Module,
  ResolvedConstructor
> {
  abstract Compiler getCompiler();

  @Override
  protected final String nameForImport(Import.Module imp) {
    return imp.name().name();
  }

  @Override
  protected final java.util.List<String> partsForImport(Import.Module imp) {
    return CollectionConverters.SeqHasAsJava(imp.name().parts().map(n -> n.name())).asJava();
  }

  @Override
  protected final String nameForExport(Export.Module ex) {
    return ex.name().name();
  }

  @Override
  protected final String nameForType(BindingsMap.ResolvedType e) {
    return e.qualifiedName().item();
  }

  @Override
  protected String nameForConstructor(ResolvedConstructor cons) {
    return cons.qualifiedName().item();
  }

  @Override
  protected final java.util.List<Export.Module> exportsFor(Module module, String impName) {
    java.util.List<Export.Module> exp = CollectionConverters.SeqHasAsJava(module.exports()).asJava().stream().map(e -> switch (e) {
      case Export.Module ex when ex.name().name().equals(impName) -> ex;
      case null, default -> null;
    }).filter(Objects::nonNull).toList();
    return exp;
  }

  @Override
  protected final java.util.List<String> onlyNames(Export.Module ex) {
    if (ex.onlyNames().isEmpty()) {
      return null;
    }
    var list = CollectionConverters.SeqHasAsJava(ex.onlyNames().get().map(n -> n.name())).asJava();
    return list;
  }

  @Override
  protected final java.util.List<BindingsMap.ResolvedType> definedEntities(Import.Module name) {
    var parts = partsForImport(name);
    var last = parts.size() - 1;
    var modName = String.join(".", parts.subList(0, last).stream().toList());
    var compiler = this.getCompiler();
    var optionMod = compiler.getModule(modName);
    if (optionMod.isEmpty()) {
      return null;
    }
    var mod = optionMod.get();
    compiler.ensureParsed(mod);
    var bindingsMap = loadBindingsMap(mod);
    var entitiesStream = bindingsMap.definedEntities().map(e -> switch (e) {
      case BindingsMap.Type t -> {
        assert e.name().equals(t.name()) : e.name() + " != " + t.name();
        var res = new ResolvedType(new BindingsMap$ModuleReference$Concrete(mod), t);
        assert e.name().equals(res.tp().name()) : e.name() + " != " + res.tp().name();
        yield res;
      }
      case null, default -> null;
    }).filter(Objects::nonNull);
    var entities = CollectionConverters.SeqHasAsJava(entitiesStream).asJava();
    return entities;
  }

  /**
   * Returns list of constructors for the given import.
   * @param imp The import is treated as an import of a constructor from a type.
   *            The last part is constructor, the second to last is type,
   *            the third to last is module.
   * @return null if the import is not a constructor import.
   */
  @Override
  protected java.util.List<ResolvedConstructor> definedConstructors(Import.Module imp) {
    var parts = partsForImport(imp);
    if (parts.size() < 3) {
      return null;
    }
    var typeName = parts.get(parts.size() - 2);
    var modFullName = parts
        .stream()
        .limit(parts.size() - 2)
        .collect(Collectors.joining("."));
    var compiler = getCompiler();
    var optionMod = compiler.getModule(modFullName);
    if (optionMod.isEmpty()) {
      return null;
    }
    var mod = optionMod.get();
    compiler.ensureParsed(mod);
    var bindingsMap = loadBindingsMap(mod);
    var foundType = scala.jdk.javaapi.CollectionConverters.asJava(bindingsMap.definedEntities())
        .stream()
        .map(e -> {
          if (e instanceof BindingsMap.Type tp) {
            return tp;
          }
          return null;
        })
        .filter(Objects::nonNull)
        .filter(tp -> tp.name().equals(typeName))
        .findFirst();
    if (foundType.isEmpty()) {
      return null;
    }
    var tp = foundType.get();
    var resolvedType = new BindingsMap.ResolvedType(
        new BindingsMap$ModuleReference$Concrete(mod),
        tp
    );
    return scala.jdk.javaapi.CollectionConverters.asJava(tp.members())
        .stream()
        .map(cons -> new ResolvedConstructor(resolvedType, cons))
        .collect(Collectors.toUnmodifiableList());
  }

  @Override
  protected final CompilerContext.Module loadLibraryModule(LibraryName libraryName, String moduleName) throws IOException {
    var compiler = this.getCompiler();
    var repo = compiler.packageRepository();
    var foundLib = repo.ensurePackageIsLoaded(libraryName);
    if (foundLib.isRight()) {
      var moduleOption = compiler.getModule(moduleName);
      if (moduleOption.isDefined()) {
        return moduleOption.get();
      } else {
        return null;
      }
    } else {
      throw new IOException(foundLib.left().getOrElse(null).toString());
    }
  }

  @Override
  protected final Tuple2<Import, Option<BindingsMap.ResolvedImport>> createResolvedImport(Import.Module imp, java.util.List<Export.Module> exp, CompilerContext.Module m) {
    scala.Option<org.enso.compiler.data.BindingsMap.ResolvedImport> someBinding = Option.apply(new BindingsMap.ResolvedImport(imp, toScalaList(exp), new BindingsMap.ResolvedModule(new BindingsMap$ModuleReference$Concrete(m))));
    return new Tuple2<>(imp, someBinding);
  }

  @Override
  protected final Tuple2<Import, Option<BindingsMap.ResolvedImport>> createResolvedType(Import.Module imp, java.util.List<Export.Module> exp, BindingsMap.ResolvedType typ) {
    scala.Option<org.enso.compiler.data.BindingsMap.ResolvedImport> someBinding = Option.apply(new BindingsMap.ResolvedImport(imp, toScalaList(exp), typ));
    return new Tuple2<>(imp, someBinding);
  }

  @Override
  protected Tuple2<Import, Option<ResolvedImport>> createResolvedConstructor(Import.Module imp,
      java.util.List<Export.Module> exp, ResolvedConstructor cons) {
    scala.Option<org.enso.compiler.data.BindingsMap.ResolvedImport> someBinding = Option.apply(new BindingsMap.ResolvedImport(imp, toScalaList(exp), cons));
    return new Tuple2<>(imp, someBinding);
  }

  @Override
  protected final Tuple2<Import, Option<BindingsMap.ResolvedImport>> createErrorPackageCoundNotBeLoaded(Import.Module imp, String impName, String loadingError) {
    org.enso.compiler.core.ir.expression.errors.ImportExport importError = new ImportExport(imp, new ImportExport.PackageCouldNotBeLoaded(impName, loadingError), imp.passData(), imp.diagnostics());
    return new Tuple2<>(importError, Option.empty());
  }

  @Override
  protected final Tuple2<Import, Option<BindingsMap.ResolvedImport>> createErrorModuleDoesNotExist(Import.Module imp, String impName) {
    return new Tuple2<>(new ImportExport(imp, new ImportExport.ModuleDoesNotExist(impName), imp.passData(), imp.diagnostics()), Option.empty());
  }

  private BindingsMap loadBindingsMap(CompilerContext.Module mod) {
    var bindingsMap = mod.getBindingsMap();
    if (bindingsMap == null) {
      getCompiler().context().updateModule(mod, u -> {
        u.invalidateCache();
        u.ir(null);
        u.compilationStage(CompilationStage.INITIAL);
      });
      getCompiler().ensureParsed(mod, false);
      bindingsMap = mod.getBindingsMap();
    }
    return bindingsMap;
  }

  private static <T> List<T> toScalaList(java.util.List<T> qualifiedConflicts) {
    return CollectionConverters.ListHasAsScala(qualifiedConflicts).asScala().toList();
  }
}
