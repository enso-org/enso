package org.enso.compiler.pass.lint.unusedimports;

import static scala.jdk.javaapi.CollectionConverters.asJava;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Warning.UnusedImport;
import org.enso.compiler.core.ir.Warning.UnusedSymbolsFromImport;
import org.enso.compiler.core.ir.module.scope.Import;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    var lazyFile = new LazyFile(modulePath);
    var replacements = collectReplacements(lazyFile, moduleCtx);
    var replacementsSorted = reverseSort(replacements);
    if (!replacementsSorted.isEmpty()) {
      LOGGER.trace("Replacements: {}", replacementsSorted);
      replaceLines(lazyFile, replacementsSorted);
    }
  }

  private static List<LineReplacement> collectReplacements(
      LazyFile lazyFile, CompilerContext.Module moduleCtx) {
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
              var loc = imp.identifiedLocation();
              assert loc != null;
              var lineIdx = moduleCtx.findLine(loc);
              var line = lazyFile.lines().get(lineIdx - 1);
              var replacement =
                  replacementForUnusedSymbols(unusedSymsImp, (Import.Module) imp, lineIdx, line);
              replacements.add(replacement);
            }
            default -> {}
          }
        }
      }
    }
    return replacements;
  }

  /**
   * @param unusedSymsImp Warning attached to the IR
   * @param impIr The original Import IR
   * @param lineIdx Index of the line in the file
   * @param importStatement String representation of the import ir. This is important because if the
   *     import statement is {@code from project... import ...}, then the {@code project} keyword
   *     would otherwise be lost. Note that if the statement was {@code from project.A.B import C,
   *     D, E}, we want to replace it with something like {@code from project.A.B import C, D} and
   *     not with {@code from Standard.Base.A.B import C, D}
   * @return
   */
  private static LineReplacement replacementForUnusedSymbols(
      UnusedSymbolsFromImport unusedSymsImp,
      Import.Module impIr,
      int lineIdx,
      String importStatement) {
    assert impIr.onlyNames().isDefined();
    var onlyNames = impIr.onlyNames().get();
    var unusedSyms = unqualified(asJava(unusedSymsImp.unusedSymbols()));
    assert unusedSyms.size() <= onlyNames.size();
    var usedSyms = onlyNames.filterNot(onlyName -> unusedSyms.contains(onlyName.name()));
    if (usedSyms.isEmpty()) {
      // Remove the line - no used symbols
      return new LineReplacement(lineIdx, null);
    } else {
      var pat = Pattern.compile("from\\s+(.+)\\s+import(.+)$");
      var matcher = pat.matcher(importStatement);
      var found = matcher.find();
      assert found;
      var module = matcher.group(1);
      var usedSymsStr = usedSyms.map(Name.Literal::name).mkString(", ");
      var repl = "from " + module + " import " + usedSymsStr;
      return new LineReplacement(lineIdx, repl);
    }
  }

  /**
   * @param replacements Locations sorted in reverse order, so they can be removed in one pass
   */
  private static void replaceLines(LazyFile lazyFile, List<LineReplacement> replacements) {
    assert !replacements.isEmpty();
    var oldLines = lazyFile.lines();
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
    try {
      Files.write(lazyFile.path, newLines);
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

  private static final class LazyFile {
    private final Path path;
    private List<String> lines;

    private LazyFile(Path path) {
      this.path = path;
    }

    List<String> lines() {
      if (lines == null) {
        try {
          lines = Files.readAllLines(path);
        } catch (IOException e) {
          throw new IllegalStateException(e);
        }
      }
      return lines;
    }
  }
}
