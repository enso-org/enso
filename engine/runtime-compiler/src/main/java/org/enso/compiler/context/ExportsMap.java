package org.enso.compiler.context;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.ExportedSymbol;
import org.enso.polyglot.ModuleExports;
import org.enso.polyglot.Suggestion;
import scala.runtime.BoxedUnit;

public final class ExportsMap {

  private static final String MODULE_MAIN = "Main";
  private static final String TYPE_SUFFIX = "type";

  private static final Comparator<QualifiedName> EXPORTS_COMPARATOR =
      Comparator.comparingInt(ExportsMap::length);

  private final Map<ExportedSymbol, SortedSet<QualifiedName>> exportsMap;

  public ExportsMap() {
    this.exportsMap = new HashMap<>();
  }

  public ExportsMap(Map<ExportedSymbol, SortedSet<QualifiedName>> exportsMap) {
    this.exportsMap = exportsMap;
  }

  public void add(ExportedSymbol symbol, QualifiedName moduleName) {
    exportsMap.compute(symbol, (_, v) -> {
      var set = v == null ? new TreeSet<>(EXPORTS_COMPARATOR) : v;
      set.add(moduleName);
      return set;
    });
  }

  public void addAll(QualifiedName moduleName, ModuleExports moduleExports) {
    moduleExports
        .symbols()
        .foreach(
            symbol -> {
              add(symbol, moduleName);
              return BoxedUnit.UNIT;
            });
  }

  public SortedSet<QualifiedName> get(Suggestion suggestion) {
    return ExportedSymbol.fromSuggestion(suggestion)
        .map(symbol -> exportsMap.getOrDefault(symbol, new TreeSet<>(EXPORTS_COMPARATOR)))
        .getOrElse(() -> new TreeSet<>(EXPORTS_COMPARATOR));
  }

  private static int length(QualifiedName qualifiedName) {
    QualifiedName name =
        qualifiedName.item().equals(TYPE_SUFFIX) ? qualifiedName.getParent().get() : qualifiedName;
    return name.item().equals(MODULE_MAIN) ? name.path().length() : name.path().length() + 1;
  }
}
