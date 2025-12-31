package org.enso.compiler.core.ir;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface Type extends Expression {
  @Override
  Type mapExpressions(java.util.function.Function<Expression, Expression> fn);

  @Override
  Type setLocation(Option<IdentifiedLocation> location);

  @Override
  Type duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Type.class})
  final class Function extends TypeFunctionGen {
    @GenerateFields
    public Function(
        @IRChild List<Expression> args,
        @IRChild Expression result,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(args, result, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().args(emptyScalaList());
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      return args().map(IR::showCode).mkString(" -> ") + " -> " + result().showCode();
    }
  }

  /** The ascription of a type to a value */
  @GenerateIR(interfaces = {Type.class, Definition.class, IRKind.Primitive.class})
  final class Ascription extends TypeAscriptionGen {
    public static final String NAME = ":";

    /**
     * @param typed the expression being ascribed a type
     * @param signature the signature being ascribed to `typed`
     */
    @GenerateFields
    public Ascription(
        @IRChild Expression typed,
        @IRChild Expression signature,
        @IRField AscriptionReason reason,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(typed, signature, reason, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().reason(AscriptionReason.empty());
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    public Ascription copyWithTyped(Expression typed) {
      return new Builder(this).typed(typed).build();
    }

    @Override
    public String showCode(int indent) {
      return typed().showCode(indent) + " : " + signature().showCode(indent);
    }
  }

  /**
   * A representation of the {@code in} portion of a type signature that represents the ascription
   * of a monadic context.
   */
  @GenerateIR(interfaces = {Type.class, IRKind.Primitive.class})
  final class Context extends TypeContextGen {
    public static final String NAME = "in";

    /**
     * @param typed the type being ascribed a monadic context
     * @param context the context being ascribed to `typed`
     */
    @GenerateFields
    public Context(
        @IRChild Expression typed,
        @IRChild Expression context,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(typed, context, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      return typed().showCode(indent) + " in " + context().showCode(indent);
    }
  }

  /** Represents the ascription of an error context to an expression */
  @GenerateIR(interfaces = {Type.class, IRKind.Primitive.class})
  final class Error extends TypeErrorGen {
    public static final String NAME = "!";

    /**
     * @param typed The expression being ascribed an error context.
     * @param error The error being ascribed.
     */
    @GenerateFields
    public Error(
        @IRChild Expression typed,
        @IRChild Expression error,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(typed, error, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      return "(" + typed().showCode(indent) + " ! " + error().showCode(indent) + ")";
    }
  }

  private static <T> scala.collection.immutable.List<T> emptyScalaList() {
    return scala.collection.immutable.List$.MODULE$.empty();
  }
}
