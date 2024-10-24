package org.enso.compiler.core.ir;

import java.util.List;
import org.enso.compiler.core.ir.module.scope.JExport;
import org.enso.compiler.core.ir.module.scope.JImport;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

@IRNode
public interface JModule {
  @IRChild
  List<JImport> imports();

  @IRChild
  List<JExport> exports();
}
