package org.enso.compiler.core.ir.expression.errors;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.expression.Error;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Function1;
import scala.Option;

/** An error representing a type signature not associated with a binding of some kind. */
@GenerateIR(interfaces = {Error.class, IRKind.Primitive.class, Definition.class})
public final class UnexpectedTypeSignature extends UnexpectedTypeSignatureErrorGen {
  /**
   * @param ir The erroneous signature
   */
  @GenerateFields
  public UnexpectedTypeSignature(@IRField IR ir, MetadataStorage passData) {
    super(ir, passData);
  }

  public static UnexpectedTypeSignature create(IR ir) {
    return builder().ir(ir).build();
  }

  @Override
  public String message(Function1<IdentifiedLocation, String> source) {
    return "Unexpected type signature";
  }

  @Override
  public IdentifiedLocation identifiedLocation() {
    return ir().identifiedLocation();
  }

  @Override
  public UnexpectedTypeSignature mapExpressions(Function<Expression, Expression> fn) {
    return this;
  }

  @Override
  public UnexpectedTypeSignature setLocation(Option<IdentifiedLocation> location) {
    return this;
  }

  @Override
  public Object[] diagnosticKeys() {
    return new Object[] {ir()};
  }

  @Override
  public String showCode(int indent) {
    return "(Unexpected.TypeSignature " + ir().showCode(indent) + ")";
  }
}
