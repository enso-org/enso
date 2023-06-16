package org.enso.compiler.context;

import java.util.HashMap;
import java.util.Map;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.ExportedSymbol;
import org.enso.polyglot.ModuleExports;
import org.enso.polyglot.Suggestion;
import scala.Option;
import scala.runtime.BoxedUnit;

public final class ExportsMap {

  private static final String MODULE_MAIN = "Main";
  private static final String TYPE_SUFFIX = "type";

  private final Map<ExportedSymbol, QualifiedName> exportsMap;

  public ExportsMap() {
    this.exportsMap = new HashMap<>();
  }

  public ExportsMap(Map<ExportedSymbol, QualifiedName> exportsMap) {
    this.exportsMap = exportsMap;
  }

  public void add(ExportedSymbol symbol, QualifiedName moduleName) {
    exportsMap.merge(symbol, moduleName, ExportsMap::getShortest);
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

  public QualifiedName get(ExportedSymbol symbol) {
    return exportsMap.get(symbol);
  }

  public QualifiedName get(Suggestion suggestion) {
    return ExportedSymbol.fromSuggestion(suggestion)
        .flatMap(symbol -> Option.apply(exportsMap.get(symbol)))
        .getOrElse(() -> exportsMap.get(ExportedSymbol.suggestionModule(suggestion)));
  }

  private static QualifiedName getShortest(QualifiedName name1, QualifiedName name2) {
    return length(name1) <= length(name2) ? name1 : name2;
  }

  private static int length(QualifiedName qualifiedName) {
    QualifiedName name =
        qualifiedName.item().equals(TYPE_SUFFIX) ? qualifiedName.getParent().get() : qualifiedName;
    return name.item().equals(MODULE_MAIN) ? name.path().length() : name.path().length() + 1;
  }
}
