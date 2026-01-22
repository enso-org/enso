package org.enso.compiler.core.ir;

import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.PartialFunction;
import scala.collection.immutable.List;

public interface Expression extends IR {

  @Override
  Expression mapExpressions(Function<Expression, Expression> fn);

  /**
   * Performs a recursive traversal of the IR, potentially transforming it.
   *
   * @param fn the function to apply across the IR
   * @return the IR, potentially transformed
   */
  default Expression transformExpressions(PartialFunction<Expression, Expression> fn) {
    if (fn.isDefinedAt(this)) {
      return fn.apply(this);
    } else {
      return mapExpressions(e -> e.transformExpressions(fn));
    }
  }

  @Override
  Expression setLocation(Option<IdentifiedLocation> location);

  default Expression duplicate() {
    return duplicate(true, true, true, false);
  }

  @Override
  Expression duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Expression.class, IRKind.Primitive.class})
  final class Block extends ExpressionBlockGen {
    /**
     * @param expressions the expressions in the block
     * @param returnValue the final expression in the block
     * @param suspended whether or not the block is suspended
     */
    @GenerateFields
    public Block(
        @IRChild List<Expression> expressions,
        @IRChild Expression returnValue,
        @IRField boolean suspended,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(expressions, returnValue, suspended, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder().expressions(nil()).suspended(false);
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @SuppressWarnings("unchecked")
    private static <T> scala.collection.immutable.List<T> nil() {
      return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
    }

    @Override
    public String showCode(int indent) {
      var newIndent = indent + indentLevel;
      var expressionsStr =
          expressions().map(e -> IR.mkIndent(newIndent) + e.showCode(newIndent)).mkString("\n");
      var returnStr = IR.mkIndent(newIndent) + returnValue().showCode(newIndent);
      return "\n" + expressionsStr + "\n" + returnStr;
    }
  }

  /**
   * A binding expression of the form {@code name = expr}.
   *
   * <p>To create a binding that binds no available name, set the name of the binding to an {@link
   * Name.Blank} (e.g. _ = foo a b).
   */
  @GenerateIR(interfaces = {Expression.class, IRKind.Primitive.class})
  final class Binding extends ExpressionBindingGen {

    /**
     * @param name the name being bound to
     * @param expression the expression being bound to `name`
     */
    @GenerateFields
    public Binding(
        @IRChild Name name,
        @IRChild Expression expression,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData) {
      super(name, expression, identifiedLocation, passData);
    }

    public static Builder builder() {
      return new Builder();
    }

    public Builder copyBuilder() {
      return new Builder(this);
    }

    @Override
    public Binding mapExpressions(Function<Expression, Expression> fn) {
      var newName = name().mapExpressions(fn);
      var newExpr = fn.apply(expression());
      return new Builder(this).name(newName).expression(newExpr).build();
    }

    @Override
    public String showCode(int indent) {
      return name().showCode(indent) + " = " + expression().showCode(indent);
    }
  }
}
