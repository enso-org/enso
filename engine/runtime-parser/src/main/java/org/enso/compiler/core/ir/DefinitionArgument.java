package org.enso.compiler.core.ir;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;

public interface DefinitionArgument extends IR {
  Name name();

  Option<Expression> ascribedType();

  Option<Expression> defaultValue();

  boolean suspended();

  @Override
  DefinitionArgument mapExpressions(Function<Expression, Expression> fn);

  @Override
  DefinitionArgument duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  DefinitionArgument withName(Name ir);

  @GenerateIR(interfaces = {DefinitionArgument.class})
  final class Specified extends DefinitionArgumentSpecifiedGen {
    @GenerateFields
    public Specified(
        @IRChild Name name,
        @IRChild Option<Expression> ascribedType,
        @IRChild Option<Expression> defaultValue,
        @IRField boolean suspended,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(name, ascribedType, defaultValue, suspended, identifiedLocation, passData, diagnostics);
    }

    public Specified(
        Name name,
        Option<Expression> ascribedType,
        Option<Expression> defaultValue,
        boolean suspended,
        IdentifiedLocation identifiedLocation) {
      this(
          name,
          ascribedType,
          defaultValue,
          suspended,
          identifiedLocation,
          new MetadataStorage(),
          null);
    }

    public Specified(
        Name name,
        Option<Expression> ascribedType,
        Option<Expression> defaultValue,
        boolean suspended,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      this(name, ascribedType, defaultValue, suspended, identifiedLocation, passData, null);
    }

    public Specified(Name name) {
      this(name, Option.empty(), Option.empty(), false, null, new MetadataStorage());
    }

    @Override
    public String showCode(int indent) {
      String withoutLazy;
      if (defaultValue().isDefined() && ascribedType().isDefined()) {
        var name = name().showCode(indent);
        var typeExpr = ascribedType().get().showCode(indent);
        var defaultExpr = defaultValue().get().showCode(indent);
        withoutLazy = String.format("(%s : (%s) = (%s))", name, typeExpr, defaultExpr);
      } else if (defaultValue().isDefined()) {
        var name = name().showCode(indent);
        var defaultExpr = defaultValue().get().showCode(indent);
        withoutLazy = String.format("(%s = %s)", name, defaultExpr);
      } else if (ascribedType().isDefined()) {
        var name = name().showCode(indent);
        var typeExpr = ascribedType().get().showCode(indent);
        withoutLazy = String.format("((%s : %s))", name, typeExpr);
      } else {
        withoutLazy = name().showCode(indent);
      }
      if (suspended()) {
        return "~" + withoutLazy;
      } else {
        return withoutLazy;
      }
    }

    @Override
    public DefinitionArgument withName(Name ir) {
      return copy(
          diagnostics, passData, location, id, ir, ascribedType(), defaultValue(), suspended());
    }

    public Specified copyWithAscribedType(Option<Expression> ascribedType) {
      return copy(
          diagnostics, passData, location, id, name(), ascribedType, defaultValue(), suspended());
    }

    public Specified copyWithDefaultValue(Option<Expression> defaultValue) {
      return copy(
          diagnostics, passData, location, id, name(), ascribedType(), defaultValue, suspended());
    }

    public Specified copyWithSuspended(boolean suspended) {
      return copy(
          diagnostics, passData, location, id, name(), ascribedType(), defaultValue(), suspended);
    }

    public Specified copy(Option<Expression> defaultValue, Option<Expression> ascribedType) {
      return copy(
          diagnostics, passData, location, id, name(), ascribedType, defaultValue, suspended());
    }

    public Specified copy(Name name, Option<Expression> defaultValue) {
      return copy(
          diagnostics, passData, location, id, name, ascribedType(), defaultValue, suspended());
    }

    public Specified copyWithNameAndAscribedType(Name name, Option<Expression> ascribedType) {
      return copy(
          diagnostics, passData, location, id, name, ascribedType, defaultValue(), suspended());
    }
  }
}
