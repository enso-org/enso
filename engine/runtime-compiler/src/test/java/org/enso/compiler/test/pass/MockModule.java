package org.enso.compiler.test.pass;

import static org.enso.scala.wrapper.ScalaConversions.cons;
import static org.enso.scala.wrapper.ScalaConversions.nil;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.test.ir.IRUtils;
import org.enso.persist.Persistance.Reference;
import scala.Option;

final class MockModule {
  static Module createWithSingleMethod(Expression methodBody) {
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
    return new Module(
        nil(), nil(), cons(methodIr, nil()), false, null, new MetadataStorage(), null);
  }
}
