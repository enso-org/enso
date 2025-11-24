package org.enso.compiler.core.ir.expression;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;

/** Enso if/then(else) expression. */
@GenerateIR(interfaces = {Expression.class})
public final class IfThenElse extends IfThenElseGen {
  @GenerateFields
  public IfThenElse(
      @IRChild Expression condition,
      @IRChild Expression trueBranch,
      @IRChild(required = false) Expression falseBranchOrNull,
      IdentifiedLocation identifiedLocation,
      MetadataStorage passData) {
    super(condition, trueBranch, falseBranchOrNull, identifiedLocation, passData);
  }

  public Expression cond() {
    return condition();
  }

  public IfThenElse copy(Expression cond) {
    return builder(this).condition(cond).build();
  }

  public IfThenElse copy(Expression cond, Expression trueBranch, Expression falseBranchOrNull) {
    return builder(this)
        .condition(cond)
        .trueBranch(trueBranch)
        .falseBranchOrNull(falseBranchOrNull)
        .build();
  }

  public scala.Option<Expression> falseBranch() {
    return scala.Option.apply(falseBranchOrNull());
  }

  @Override
  public String showCode(int indent) {
    var newIndent = indent + indentLevel;
    var headerStr = "if " + cond().showCode(0) + " then\n" + trueBranch().showCode(newIndent);
    var elseStr =
        switch (falseBranchOrNull()) {
          case Expression f -> " ".repeat(indent) + "else\n" + f.showCode(newIndent);
          case null -> "";
        };
    return headerStr + "\n" + elseStr;
  }
}
