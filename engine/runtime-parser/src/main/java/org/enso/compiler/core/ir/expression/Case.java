package org.enso.compiler.core.ir.expression;

import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Pattern;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;
import scala.collection.immutable.List;

public interface Case extends Expression {

  @Override
  Case mapExpressions(Function<Expression, Expression> fn);

  @Override
  Case setLocation(Option<IdentifiedLocation> location);

  @Override
  Case duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Case.class, IRKind.Primitive.class})
  final class Expr extends CaseExprGen {
    public static Builder builder() {
      return new Builder();
    }

    @GenerateFields
    public Expr(
        @IRChild Expression scrutinee,
        @IRChild List<Branch> branches,
        @IRField boolean isNested,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(scrutinee, branches, isNested, identifiedLocation, passData, diagnostics);
    }

    public Expr copy(Expression scrutinee, List<Branch> branches, boolean isNested) {
      return new Builder(this).scrutinee(scrutinee).branches(branches).isNested(isNested).build();
    }

    public Expr copy(Expression scrutinee, List<Branch> branches) {
      return new Builder(this).scrutinee(scrutinee).branches(branches).build();
    }

    public Expr copy(List<Branch> branches) {
      return new Builder(this).branches(branches).build();
    }

    @Override
    public String showCode(int indent) {
      var newIndent = indent + indentLevel;
      var headerStr = "case " + scrutinee().showCode(indent) + " of";
      var branchesStr =
          branches().map(br -> IR.mkIndent(newIndent) + br.showCode(newIndent)).mkString("\n");
      return headerStr + "\n" + branchesStr;
    }
  }

  @GenerateIR(interfaces = {Case.class, IRKind.Primitive.class})
  final class Branch extends CaseBranchGen {
    public static Builder builder() {
      return new Builder();
    }

    @GenerateFields
    public Branch(
        @IRChild Pattern pattern,
        @IRChild Expression expression,
        @IRField boolean terminalBranch,
        IdentifiedLocation identifiedLocation,
        MetadataStorage passData,
        DiagnosticStorage diagnostics) {
      super(pattern, expression, terminalBranch, identifiedLocation, passData, diagnostics);
    }

    public Branch copy(Pattern pattern, Expression expression, boolean terminalBranch) {
      return new Builder(this)
          .pattern(pattern)
          .expression(expression)
          .terminalBranch(terminalBranch)
          .build();
    }

    public Branch copy(Expression expression, IdentifiedLocation identifiedLocation) {
      return new Builder(this).expression(expression).location(identifiedLocation).build();
    }

    public UUID id() {
      // TBD: this method should return null when there is no ID
      // but there is no way to check if the ID is null or not now
      // when the id field is private in a superclass
      return super.getId();
    }

    @Override
    public String showCode(int indent) {
      var newIndent = indent + indentLevel;
      String bodyStr;
      if (expression() instanceof Expression.Block block) {
        bodyStr = "\n" + IR.mkIndent(newIndent) + block.showCode(newIndent);
      } else {
        bodyStr = expression().showCode(indent);
      }
      return pattern().showCode(indent) + "->" + bodyStr;
    }
  }
}
