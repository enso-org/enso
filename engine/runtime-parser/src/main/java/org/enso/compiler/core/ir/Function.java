package org.enso.compiler.core.ir;

import org.enso.compiler.core.IR;
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
   * Whether or not the function <i>can</i> be tail-call optimised.
   *
   * <p>Please note that this being set to `true` does not <i>guarantee</i> that the function is
   * optimised.
   */
  boolean canBeTCO();

  /** Whether the method is project-private. */
  boolean isPrivate();

  @Override
  default String showCode(int indent) {
    var args = arguments().map(arg -> arg.showCode(indent)).mkString(" ");
    var bodyStr =
        switch (body()) {
          case Expression.Block block -> "\n" + block.showCode(indent);
          default -> body().showCode(indent);
        };
    return args + " -> " + bodyStr;
  }

  static boolean computeIsStatic(IR body) {
    if (body instanceof Function.Lambda function) {
      var firstArgName = function.arguments().headOption().map(DefinitionArgument::name);
      if (firstArgName.isDefined() && firstArgName.get() instanceof Name.Self self) {
        return self.synthetic();
      }
    }
    // if it's not a function, it has no arguments, therefore no `self`.
    return false;
  }

  /**
   * The primitive function type in Enso: `->`.
   *
   * <p>It should be noted that while the _surface_ language does not support multi-argument
   * lambdas, our internal representation does so to allow for better optimisation.
   */
  @GenerateIR(interfaces = {Function.class, IRKind.Primitive.class})
  final class Lambda extends FunctionLambdaGen {
    /**
     * @param arguments the arguments to the lambda
     * @param bodyReference the body of the lambda, stored as a reference to ensure laziness of
     *     storage
     * @param identifiedLocation the source location that the node corresponds to
     * @param canBeTCO whether or not the function can be tail-call optimised
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Lambda(
        @IRChild List<DefinitionArgument> arguments,
        @IRChild Persistance.Reference<Expression> bodyReference,
        @IRField boolean canBeTCO,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(arguments, bodyReference, canBeTCO, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder().canBeTCO(true);
    }

    public static Builder builder(Lambda copy) {
      return new Builder(copy);
    }

    public Lambda copyWithBody(Expression newBody) {
      return new Builder(this).bodyReference(Persistance.Reference.of(newBody)).build();
    }

    public Lambda copyWithArgumentsAndBody(List<DefinitionArgument> arguments, Expression body) {
      return new Builder(this)
          .arguments(arguments)
          .bodyReference(Persistance.Reference.of(body))
          .build();
    }

    public Lambda copyWithArguments(List<DefinitionArgument> newArgs) {
      return new Builder(this).arguments(newArgs).build();
    }

    @Override
    public Expression body() {
      return bodyReference().get(Expression.class);
    }

    @Override
    public boolean isPrivate() {
      return false;
    }
  }

  /** A representation of the syntactic sugar for defining functions. */
  @GenerateIR(interfaces = {Function.class, IRKind.Sugar.class})
  final class Binding extends FunctionBindingGen {

    /**
     * @param name the name of the function
     * @param arguments the arguments to the function
     * @param body the body of the function
     * @param isPrivate Whether the function is project-private
     * @param identifiedLocation the source location that the node corresponds to
     * @param canBeTCO whether or not the function can be tail-call optimised
     * @param passData the pass metadata associated with this node
     */
    @GenerateFields
    public Binding(
        @IRChild Name name,
        @IRChild List<DefinitionArgument> arguments,
        @IRChild Expression body,
        @IRField boolean isPrivate,
        @IRField boolean canBeTCO,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(name, arguments, body, isPrivate, canBeTCO, identifiedLocation, passData, diagnostics);
    }

    public static Builder builder() {
      return new Builder().canBeTCO(true);
    }

    public static Builder builder(Function.Binding copy) {
      return new Builder(copy);
    }

    public Function.Binding copyWithBody(Expression body) {
      return new Builder(this).body(body).build();
    }
  }
}
