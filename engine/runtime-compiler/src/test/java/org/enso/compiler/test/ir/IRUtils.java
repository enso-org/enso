package org.enso.compiler.test.ir;

import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.Operator;
import scala.Option;

public final class IRUtils {
  private IRUtils() {}

  public static Name.Literal literal(String lit) {
    return new Name.Literal(lit, false, null, Option.empty(), new MetadataStorage());
  }

  public static Name.Literal literal(String lit, MetadataStorage passData) {
    return new Name.Literal(lit, false, null, Option.empty(), passData);
  }

  public static Empty emptyIr() {
    return Empty.builder().build();
  }

  public static DefinitionArgument.Specified defArg(Name name) {
    return DefinitionArgument.Specified.builder()
        .name(name)
        .ascribedType(Option.empty())
        .defaultValue(Option.empty())
        .build();
  }

  public static Operator.Binary binaryOperator(
      CallArgument left, CallArgument right, Name operator) {
    return new Operator.Binary(left, operator, right, null, new MetadataStorage());
  }

  public static CallArgument.Specified callArg(Name name) {
    return CallArgument.Specified.builder().value(name).name(Option.empty()).build();
  }
}
