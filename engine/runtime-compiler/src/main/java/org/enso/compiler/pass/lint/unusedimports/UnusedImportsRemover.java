package org.enso.compiler.pass.lint.unusedimports;

import static scala.jdk.javaapi.CollectionConverters.asJava;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Warning.UnusedImport;
import org.enso.compiler.core.ir.Warning.UnusedSymbolsFromImport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;

/**
 * Removes unused imports from a module - rewrites the file. Either removes the line with the import
 * or replaces it with a new one with only the used symbols.
 */
public final class UnusedImportsRemover {
  private static final Logger LOGGER = LoggerFactory.getLogger(UnusedImportsRemover.class);

  /**
   * Remove all the unused imports from the module - rewrites the file. If there are no unused
   * imports, is a no-op.
   *
   * @param modulePath Path to the module.
   * @param moduleCtx
   */
  public static void removeUnusedImports(Path modulePath, CompilerContext.Module moduleCtx) {
    LOGGER.debug("About to remove unused imports from module {}", moduleCtx.getName());
    assert modulePath.toFile().exists();
    var replacements = collectReplacements(moduleCtx);
    var replacementsSorted = reverseSort(replacements);
    if (!replacementsSorted.isEmpty()) {
      LOGGER.trace("Replacements: {}", replacementsSorted);
      replaceLines(modulePath, replacementsSorted);
    }
  }

  private static List<LineReplacement> collectReplacements(CompilerContext.Module moduleCtx) {
    var modIr = moduleCtx.getIr();
    var replacements = new ArrayList<LineReplacement>();
    for (var imp : asJava(modIr.imports())) {
      if (imp.diagnostics() != null) {
        for (var diag : asJava(imp.diagnostics().toList())) {
          switch (diag) {
            case UnusedImport unusedImp -> {
              // Remove the whole line
              var loc = unusedImp.identifiedLocation();
              assert loc != null;
              var lineIdx = moduleCtx.findLine(loc);
              replacements.add(new LineReplacement(lineIdx, null));
            }
            case UnusedSymbolsFromImport unusedSymsImp -> {
              // Replace the line with a more specific import
              var unusedSyms = unqualified(asJava(unusedSymsImp.unusedSymbols()));
              assert imp instanceof Import.Module;
              var impMod = (Import.Module) imp;
              assert impMod.onlyNames().isDefined();
              var onlyNames = impMod.onlyNames().get();
              assert unusedSyms.size() <= onlyNames.size();
              var onlyNamesToRetain =
                  onlyNames.filterNot(onlyName -> unusedSyms.contains(onlyName.name()));
              Option<scala.collection.immutable.List<Name.Literal>> onlyNamesToRetainOpt =
                  onlyNamesToRetain.isEmpty() ? Option.empty() : Option.apply(onlyNamesToRetain);
              var newImp = Import.Module.builder(impMod).onlyNames(onlyNamesToRetainOpt).build();
              var loc = impMod.identifiedLocation();
              assert loc != null;
              var lineIdx = moduleCtx.findLine(loc);
              var replacement = new LineReplacement(lineIdx, newImp.showCode());
              replacements.add(replacement);
            }
            default -> {}
          }
          ;
        }
      }
    }
    return replacements;
  }

  /**
   * @param path
   * @param replacements Locations sorted in reverse order, so they can be removed in one pass
   */
  private static void replaceLines(Path path, List<LineReplacement> replacements) {
    assert !replacements.isEmpty();
    try {
      var oldLines = Files.readAllLines(path);
      var newLines = new ArrayList<>(oldLines);
      for (var replacement : replacements) {
        var oldLinesIdx = replacement.lineIdx - 1;
        var oldLine = oldLines.get(oldLinesIdx);
        if (replacement.replacement != null) {
          LOGGER.trace(
              "Replacing line [{}] '{}' with '{}'", oldLinesIdx, oldLine, replacement.replacement);
          newLines.set(oldLinesIdx, replacement.replacement);
        } else {
          LOGGER.trace("Removing line [{}] '{}'", oldLinesIdx, oldLine);
          newLines.remove(oldLinesIdx);
        }
      }
      Files.write(path, newLines);
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  private static List<String> unqualified(List<String> list) {
    return list.stream().map(s -> s.substring(s.lastIndexOf('.') + 1)).toList();
  }

  private static List<LineReplacement> reverseSort(List<LineReplacement> locations) {
    return locations.stream()
        .sorted((l1, l2) -> Integer.compare(l2.lineIdx(), l1.lineIdx()))
        .toList();
  }

  /**
   * @param lineIdx
   * @param replacement If null, the line should be removed completely
   */
  private record LineReplacement(int lineIdx, String replacement) {}
}
