package org.enso.compiler.test.pass;

import static org.enso.scala.wrapper.ScalaConversions.cons;
import static org.enso.scala.wrapper.ScalaConversions.nil;

import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.test.ir.IRUtils;
import org.enso.persist.Persistance.Reference;
import scala.Option;
import scala.collection.immutable.List;

final class MockModule extends Module {

  private MockModule(
      List<Import> imports,
      List<Export> exports,
      List<Definition> bindings,
      boolean isPrivate,
      IdentifiedLocation identifiedLocation,
      MetadataStorage passData,
      DiagnosticStorage diagnostics) {
    super(imports, exports, bindings, isPrivate, identifiedLocation, passData, diagnostics);
  }

  static MockModule createWithSingleMethod(Expression methodBody) {
    var methodName = "test";
    var methodRef =
        new Name.MethodReference(
            Option.empty(), IRUtils.literal(methodName), null, new MetadataStorage());
    var methodIr =
        Method.Explicit.builder()
            .methodReference(methodRef)
            .bodyReference(Reference.of(methodBody))
            .isPrivate(true)
            .isStatic(false)
            .build();
    return new MockModule(
        nil(), nil(), cons(methodIr, nil()), false, null, new MetadataStorage(), null);
  }
}
