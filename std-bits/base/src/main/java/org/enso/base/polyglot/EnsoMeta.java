package org.enso.base.polyglot;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public final class EnsoMeta {
  private static Value getBindings() {
    return Context.getCurrent().getBindings("enso");
  }

  public static Value getType(String moduleName, String typeName) {
    var module = getBindings().invokeMember("get_module", moduleName);
    return module.invokeMember("get_type", typeName);
  }
}
