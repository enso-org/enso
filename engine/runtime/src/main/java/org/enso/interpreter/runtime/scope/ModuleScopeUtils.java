package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.Map;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.util.CachingSupplier;

final class ModuleScopeUtils {
  private ModuleScopeUtils() {}

  static final Type noTypeKey = Type.noType();

  static Function findMethodForType(
      Type tpe, final Map<Type, Map<String, Supplier<Function>>> m, String name) {
    Type tpeKey = tpe == null ? noTypeKey : tpe;
    var allTpeMethods = m.get(tpeKey);
    if (allTpeMethods == null) {
      return null;
    }
    var supply = allTpeMethods.get(name);
    return supply == null ? null : supply.get();
  }

  static Supplier<TruffleObject> findPolyglotSymbolSupplier(
      Map<String, Supplier<TruffleObject>> ps, String symbolName) {
    var supplier = ps.get(symbolName);
    if (supplier != null) {
      return supplier;
    }
    var ctx = EnsoContext.get(null);
    var err = ctx.getBuiltins().error().makeMissingPolyglotImportError(symbolName);
    return CachingSupplier.forValue(err);
  }
}
