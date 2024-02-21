package org.enso.interpreter.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

final class IrToTruffleUtils {
  private IrToTruffleUtils() {}

  interface MethodRegistrar {
    Type forType();

    MethodRegistrar getEigentype();

    String getName();

    QualifiedName getQualifiedName();

    void registerMethod(ModuleScope scope, String name, Supplier<Function> fn);
  }

  static MethodRegistrar wrap(Type type) {
    return new MethodRegistrar() {
      @Override
      public Type forType() {
        return type;
      }

      @Override
      public MethodRegistrar getEigentype() {
        return wrap(type.getEigentype());
      }

      @Override
      public String getName() {
        return type.getName();
      }

      @Override
      public QualifiedName getQualifiedName() {
        return type.getQualifiedName();
      }

      @Override
      public void registerMethod(ModuleScope scope, String name, Supplier<Function> fn) {
        scope.registerMethod(type, name, fn);
      }
    };
  }

  static MethodRegistrar wrapPolyglot(EnsoContext ctx, Object metaObject) {
    var iop = InteropLibrary.getUncached(metaObject);
    assert iop.isMetaObject(metaObject);
    return new MethodRegistrar() {
      @Override
      public Type forType() {
        return ctx.getBuiltins().any();
      }

      @Override
      public MethodRegistrar getEigentype() {
        return wrap(forType().getEigentype());
      }

      @Override
      public String getName() {
        return forType().getName();
      }

      @Override
      public QualifiedName getQualifiedName() {
        return forType().getQualifiedName();
      }

      @Override
      public void registerMethod(ModuleScope scope, String name, Supplier<Function> fn) {
        scope.registerPolyglotMethod(metaObject, name, fn);
      }
    };
  }
}
