package org.enso.compiler.core.ir;

import org.enso.compiler.core.ir.expression.Case;
import org.enso.persist.Persistance;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.collection.immutable.List;

public interface Function extends Expression {

  /**
   * The function arguments.
   *
   * <p>Please note that while the source language does not represent multi-argument lambdas, the
   * internal language can and does.
   */
  List<DefinitionArgument> arguments();

  /** The body of the function */
  Expression body();

  /**
   * Whether or not the function _can_ be tail-call optimised.
   *
   * <p>Please note that this being set to `true` does not _guarantee_ that the function is
   * optimised.
   */
  boolean canBeTCO();

  /** Whether the method is project-private. */
  boolean isPrivate();

  @GenerateIR(interfaces = {Function.class, IRKind.Primitive.class})
  final class Lambda extends FunctionLambdaGen {
    @GenerateFields
    public Lambda(
        @IRChild List<DefinitionArgument> arguments,
        @IRField boolean canBeTCO,
        @IRChild Persistance.Reference<Expression> body,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(arguments, canBeTCO, body, identifiedLocation, passData, diagnostics);
    }

    public Lambda(
        List<DefinitionArgument> arguments,
        boolean canBeTCO,
        Persistance.Reference<Expression> bodyReference,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      this(arguments, canBeTCO, bodyReference, identifiedLocation, passData, null);
    }

    public Lambda(
        List<DefinitionArgument> arguments,
        boolean canBeTCO,
        Persistance.Reference<Expression> bodyReference) {
      this(arguments, canBeTCO, bodyReference, null, new MetadataStorage(), null);
    }

    public Lambda(
        Case.Expr ir,
        List<DefinitionArgument> arguments,
        Expression body,
        IdentifiedLocation identifiedLocation) {
      this(
          arguments,
          true,
          Persistance.Reference.of(body),
          identifiedLocation,
          ir.passData().duplicate(),
          ir.diagnostics());
    }

    public Lambda(
        List<DefinitionArgument> arguments,
        Expression body,
        IdentifiedLocation identifiedLocation) {
      this(
          arguments,
          true,
          Persistance.Reference.of(body, true),
          identifiedLocation,
          new MetadataStorage(),
          null);
    }

    @Override
    public boolean isPrivate() {
      return false;
    }

    @Override
    public String showCode(int indent) {
      var args = arguments().map(arg -> arg.showCode(indent)).mkString(" ");
      String bodyStr;
      if (body() instanceof Expression.Block block) {
        bodyStr = "\n" + block.showCode(indent);
      } else {
        bodyStr = body().showCode(indent);
      }
      return args + " -> " + bodyStr;
    }
  }

  @GenerateIR(interfaces = {Function.class, IRKind.Sugar.class})
  final class Binding extends FunctionBindingGen {
    @GenerateFields
    public Binding(
        @IRChild Name name,
        @IRChild List<DefinitionArgument> arguments,
        @IRChild Expression body,
        @IRField boolean isPrivate,
        @IRField boolean canBeTCO,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, arguments, body, isPrivate, canBeTCO, identifiedLocation, passData);
    }

    public Binding(
        Name name,
        List<DefinitionArgument> arguments,
        Expression body,
        boolean isPrivate,
        IdentifiedLocation identifiedLocation) {
      this(name, arguments, body, isPrivate, false, identifiedLocation, new MetadataStorage());
    }

    @Override
    public String showCode(int indent) {
      var args = arguments().map(arg -> arg.showCode(indent)).mkString(" ");
      String bodyStr;
      if (body() instanceof Expression.Block block) {
        bodyStr = "\n" + block.showCode(indent);
      } else {
        bodyStr = body().showCode(indent);
      }
      return name().name() + " " + args + " = " + bodyStr;
    }
  }
}
