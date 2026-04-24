package org.enso.compiler.data;

import org.enso.compiler.data.BindingsMap.ResolvedImport;

final class BindingsMapUtil {
  static boolean importMatchesName(ResolvedImport imp, String name) {
    return imp.importDef()
        .onlyNames()
        .map(ignore -> imp.importDef().rename().exists(r -> r.name().equals(name)))
        .getOrElse(
            () -> !imp.importDef().isAll() && imp.importDef().getSimpleName().name().equals(name));
  }
}
