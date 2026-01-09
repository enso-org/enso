package org.enso.compiler.core.ir.expression;

import org.enso.compiler.core.ir.Empty;
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

  /**
   * Builds "else only" instance.
   *
   * @param value the else branch
   * @param where location of the value
   * @param meta the metadaa in the tree
   * @return insance of if/then/else
   * @see #isOnlyElse()
   */
  public static IfThenElse buildOnlyElse(
      Expression value, IdentifiedLocation where, MetadataStorage meta) {
    var emptyBlock = Empty.builder().build();
    return builder()
        .condition(emptyBlock)
        .trueBranch(emptyBlock)
        .falseBranchOrNull(value)
        .location(where)
        .passData(meta)
        .build();
  }

  /**
   * Checks whether this object represents "pending" else branch. Such a branch only makes sense if
   * merged to previous if statement.
   *
   * @return {@code true} if created by {@link #buildOnlyElse} factory
   * @see #isOnlyElse()
   */
  public boolean isOnlyElse() {
    return cond() instanceof Empty c && trueBranch() instanceof Empty t && c == t;
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
