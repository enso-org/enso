package org.enso.compiler.core.ir.module.scope.definition;

import java.util.function.Function;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.persist.Persistance;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

/** Interface representing method definitions in Enso. */
public interface Method extends Definition {

  Name.MethodReference methodReference();

  Expression body();

  boolean isPrivate();

  /** Get the type name for the method. */
  default Option<Name> typeName() {
    return methodReference().typePointer();
  }

  /** Get the name of the method */
  default Name methodName() {
    return methodReference().methodName();
  }

  @Override
  Method setLocation(Option<IdentifiedLocation> location);

  @Override
  Method mapExpressions(Function<Expression, Expression> fn);

  @Override
  Method duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /** The definition of a method for a given constructor. */
  @GenerateIR(interfaces = {Method.class, IRKind.Primitive.class})
  final class Explicit extends MethodExplicitGen {

    /**
     * @param methodReference a reference to the method being defined
     * @param bodyReference the body of the method
     * @param isStatic true if this method is static, false otherwise
     * @param isPrivate true if this method is private, false otherwise
     */
    @GenerateFields
    public Explicit(
        @IRChild Name.MethodReference methodReference,
        @IRChild Persistance.Reference<Expression> bodyReference,
        @IRField boolean isPrivate,
        @IRField boolean isStatic,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(
          methodReference,
          bodyReference,
          isPrivate,
          isStatic,
          identifiedLocation,
          passData,
          diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    /**
     * Create an {@link Explicit} object from {@link Method.Binding}.
     *
     * @param ir the method binding IR
     * @param body the method body expression
     */
    public static Explicit fromMethodBinding(Method.Binding ir, Expression body) {
      return new Builder()
          .methodReference(ir.methodReference())
          .bodyReference(Persistance.Reference.of(body, false))
          .isStatic(org.enso.compiler.core.ir.Function.computeIsStatic(body))
          .isPrivate(ir.isPrivate())
          .location(ir.identifiedLocation())
          .passData(ir.passData())
          .diagnostics(ir.diagnostics())
          .build();
    }

    @Override
    public Expression body() {
      return bodyReference().get(Expression.class);
    }

    @Override
    public String showCode(int indent) {
      String exprStr;
      if (body() instanceof Expression.Block block) {
        exprStr = "\n" + block.showCode(indent);
      } else {
        exprStr = body().showCode(indent);
      }
      return methodReference().showCode(indent) + " = " + exprStr;
    }
  }

  /** The definition of a method for a given constructor using sugared syntax. */
  @GenerateIR(interfaces = {Method.class, IRKind.Sugar.class})
  final class Binding extends MethodBindingGen {

    /**
     * @param methodReference a reference to the method being defined
     * @param arguments the arguments to the method
     * @param isPrivate if the method is declared as private (project-private). i.e. with prepended
     *     `private` keyword.
     * @param body the body of the method
     */
    @GenerateFields
    public Binding(
        @IRChild Name.MethodReference methodReference,
        @IRChild List<DefinitionArgument> arguments,
        @IRChild Expression body,
        @IRField boolean isPrivate,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(methodReference, arguments, body, isPrivate, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public String showCode(int indent) {
      String exprStr;
      if (body() instanceof Expression.Block block) {
        exprStr = "\n" + block.showCode(indent);
      } else {
        exprStr = body().showCode(indent);
      }
      var argsStr = arguments().map(arg -> arg.showCode(indent)).mkString(" ");
      return methodReference().showCode(indent) + " " + argsStr + " = " + exprStr;
    }
  }

  /** A method that represents a conversion from one type to another. */
  @GenerateIR(interfaces = {Method.class, IRKind.Primitive.class})
  final class Conversion extends MethodConversionGen {

    /**
     * @param methodReference a reference to the type on which the conversion is being defined
     * @param sourceTypeName the type of the source value for this conversion
     * @param body the body of the method
     */
    @GenerateFields
    public Conversion(
        @IRChild Name.MethodReference methodReference,
        @IRChild Expression sourceTypeName,
        @IRChild Expression body,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(methodReference, sourceTypeName, body, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    /**
     * Create a conversion method from {@link Method.Binding}.
     *
     * @param ir the method binding IR
     * @param sourceTypeName the type of the source value for this conversion
     * @param body the body of the method
     */
    public static Conversion fromMethodBinding(
        Method.Binding ir, Expression sourceTypeName, Expression body) {
      return builder()
          .methodReference(ir.methodReference())
          .sourceTypeName(sourceTypeName)
          .body(body)
          .location(ir.identifiedLocation())
          .passData(ir.passData())
          .diagnostics(ir.diagnostics())
          .build();
    }

    // Conversion methods cannot be private for now.
    @Override
    public boolean isPrivate() {
      return false;
    }

    @Override
    public String showCode(int indent) {
      String exprStr;
      if (body() instanceof Expression.Block block) {
        exprStr = "\n" + block.showCode(indent);
      } else {
        exprStr = body().showCode(indent);
      }
      return methodReference().showCode(indent) + " = " + exprStr;
    }
  }
}
