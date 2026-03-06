package org.enso.compiler.core.ir.expression;

import java.util.function.Function;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IRKind;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.runtime.parser.dsl.GenerateFields;
import org.enso.runtime.parser.dsl.GenerateIR;
import org.enso.runtime.parser.dsl.IRField;
import scala.Option;

public interface Comment extends Expression, Definition {
  @Override
  Comment mapExpressions(Function<Expression, Expression> fn);

  @Override
  Comment setLocation(Option<IdentifiedLocation> location);

  @Override
  Comment duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  @GenerateIR(interfaces = {Comment.class, IRKind.Primitive.class})
  final class Documentation extends CommentDocumentationGen {
    @GenerateFields
    public Documentation(
        @IRField String doc, IdentifiedLocation identifiedLocation, MetadataStorage passData) {
      super(doc, identifiedLocation, passData);
    }

    @Override
    public String showCode(int indent) {
      return "## " + doc();
    }
  }
}
