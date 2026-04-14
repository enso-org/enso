package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.List;
import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;

/**
 * A proxy scope delegating to the underlying module's scope. Additionally, `ImportExportScope` may
 * limit the number of types that are imported/exported.
 */
public final class ImportExportScope extends EnsoObject {

  private final Module module;
  private final List<String> onlyNames;
  private final List<String> hiddenNames;

  public ImportExportScope(
      CompilerContext.Module module, List<String> onlyNames, List<String> hiddenNames) {
    this.module = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
    this.onlyNames = onlyNames != null && !onlyNames.isEmpty() ? onlyNames : null;
    this.hiddenNames = hiddenNames != null && !hiddenNames.isEmpty() ? hiddenNames : null;
  }

  private boolean isValidTypeOrSymbol(Type type, String symbol) {
    if (onlyNames == null) {
      if (hiddenNames != null) {
        if (symbol != null && hiddenNames.contains(symbol)) {
          return false;
        }
        if (hiddenNames.contains(type.getName())) {
          return false;
        }
      }
      return true;
    } else {
      if (onlyNames.contains(type.getName()) && module.getScope().hasType(type)) {
        return true;
      }
      return symbol != null && onlyNames.contains(symbol);
    }
  }

  private boolean isValidType(Type type) {
    return isValidTypeOrSymbol(type, null);
  }

  public Function getExportedMethod(Type type, String name) {
    if (isValidTypeOrSymbol(type, name)) {
      return module.getScope().getExportedMethod(type, name);
    } else {
      return null;
    }
  }

  public Function getExportedConversion(Type target, Type source) {
    if (isValidType(target)) {
      return module.getScope().getExportedConversion(target, source);
    } else {
      return null;
    }
  }

  public Function getMethodForType(Type type, String methodName) {
    if (isValidTypeOrSymbol(type, methodName)) {
      return module.getScope().getMethodForType(type, methodName);
    } else {
      return null;
    }
  }

  public Function getConversionForType(Type target, Type source) {
    if (isValidType(target)) {
      return module.getScope().getConversionFor(target, source);
    } else {
      return null;
    }
  }

  @Override
  @TruffleBoundary
  public Object toDisplayString(boolean allowSideEffects) {
    return "ImportExportScope{" + module.getName().toString() + "}";
  }
}
