package org.enso.compiler.pass.lint.unusedimports;

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

  /**
   * Returns them <i>physical paths</i> of used symbols in the {@code importIr}, i.e., the real
   * location of the symbols.
   *
   * @param importIr
   * @return Physical path to the used symbols
   */
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
     * @param symbol Physical path to the symbol.
     */
    void addUsedSymbol(Import.Module importIr, QualifiedName symbol) {
      var usedSymbols = symbols.computeIfAbsent(importIr, k -> new HashSet<>());
      usedSymbols.add(symbol);
    }

    UsedSymbols build() {
      return new UsedSymbols(Collections.unmodifiableMap(symbols));
    }
  }
}
