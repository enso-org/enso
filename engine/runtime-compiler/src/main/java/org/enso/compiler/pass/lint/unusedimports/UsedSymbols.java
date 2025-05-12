package org.enso.compiler.pass.lint.unusedimports;

import static org.enso.scala.wrapper.ScalaConversions.asJava;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.pkg.QualifiedName;

/** All the used symbols inside one module. */
final class UsedSymbols {

  private final Map<Import, Set<QualifiedName>> symbols;

  private UsedSymbols(Map<Import, Set<QualifiedName>> symbols) {
    this.symbols = symbols;
  }

  Set<QualifiedName> getUsedSymbolsForImport(Import importIr) {
    if (!symbols.containsKey(importIr)) {
      // Try to find the import based on location.
      // It is possible that the import was replaced by a different instance
      // with same location.
      return symbols.entrySet().stream()
          .filter(entry -> UnusedImports.haveSameLocations(entry.getKey(), importIr))
          .map(Entry::getValue)
          .findFirst()
          .orElse(Set.of());
    }
    return symbols.getOrDefault(importIr, Set.of());
  }

  @Override
  public String toString() {
    var sb = new StringBuilder();
    sb.append("UsedSymbols{");
    for (var entry : symbols.entrySet()) {
      var impCode = entry.getKey().showCode();
      sb.append("'").append(impCode).append("': ").append(entry.getValue()).append(", ");
    }
    sb.append("}");
    return sb.toString();
  }

  static final class Builder {

    private final Map<Import.Module, Set<QualifiedName>> symbols = new HashMap<>();

    /**
     * Records the {@code symbol} as used by the {@code importIr}.
     *
     * <p>The symbol must be directly cotained in the import, for example symbol {@code
     * local.Proj.Module.T.Cons} is not contained in {@code from local.Proj.Module import T}, but it
     * is contained in {@code from local.Proj.Module.T import Cons}. If this is not true, {@link
     * AssertionError} is thrown.
     */
    void addUsedSymbol(Import.Module importIr, QualifiedName symbol) {
      if (importIr.onlyNames().isDefined()) {
        var onlyNames = importIr.onlyNames().get();
        var baseName = QualifiedName.fromString(importIr.name().name());
        var someNameMatches = false;
        for (var onlyName : asJava(onlyNames)) {
          var fqn = baseName.createChild(onlyName.name());
          if (fqn.equals(symbol)) {
            someNameMatches = true;
          }
        }
        if (!someNameMatches) {
          throw new AssertionError(
              "Attempting to add symbol '"
                  + symbol
                  + "' to import '"
                  + importIr.showCode()
                  + "'. But the symbol is not in the list of only names.");
        }
      } else {
        var fqn = QualifiedName.fromString(importIr.name().name());
        assert fqn.equals(symbol);
      }
      var usedSymbols = symbols.computeIfAbsent(importIr, k -> new HashSet<>());
      usedSymbols.add(symbol);
    }

    UsedSymbols build() {
      return new UsedSymbols(Collections.unmodifiableMap(symbols));
    }
  }
}
