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
public class ImportExportScope extends EnsoObject {

  private final Module module;
  private final List<String> typesOnlyNames;

  public ImportExportScope(CompilerContext.Module module, List<String> typesOnlyNames) {
    this.module = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
    this.typesOnlyNames =
        typesOnlyNames != null && !typesOnlyNames.isEmpty() ? typesOnlyNames : null;
  }

  public ImportExportScope(CompilerContext.Module module) {
    this.module = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
    this.typesOnlyNames = null;
  }

  private boolean isValidType(Type type) {
    if (typesOnlyNames == null) return true;
    return typesOnlyNames.contains(type.getName()) && module.getScope().hasType(type);
  }

  public Function getExportedMethod(Type type, String name) {
    if (isValidType(type)) {
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
    if (isValidType(type)) {
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
