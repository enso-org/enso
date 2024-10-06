package org.enso.compiler.core.ir;

import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;
import scala.collection.immutable.List;

@IRNode(name = "Module")
public abstract class JModule {
  @IRChild
  public abstract List<Import> imports();
  @IRChild
  public abstract List<Export> exports();
}
