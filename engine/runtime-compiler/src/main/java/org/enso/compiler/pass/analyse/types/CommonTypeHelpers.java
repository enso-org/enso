package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.ProcessingPass;
import scala.Option;

class CommonTypeHelpers {
  static TypeRepresentation getInferredType(Expression expression) {
    Option<ProcessingPass.Metadata> r =
        expression.passData().get(TypeInferencePropagation.INSTANCE);
    if (r.isDefined()) {
      InferredType metadata = (InferredType) r.get();
      return metadata.type();
    } else {
      return null;
    }
  }
}
