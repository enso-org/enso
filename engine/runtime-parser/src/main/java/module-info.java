module org.enso.runtime.parser {
  requires org.enso.syntax;
  requires scala.library;
  requires org.enso.persistance;
  requires static org.enso.runtime.parser.dsl;
  requires static org.enso.runtime.parser.processor;

  exports org.enso.compiler.core;
  exports org.enso.compiler.core.ir;
  exports org.enso.compiler.core.ir.expression;
  exports org.enso.compiler.core.ir.expression.errors;
  exports org.enso.compiler.core.ir.expression.warnings;
  exports org.enso.compiler.core.ir.module;
  exports org.enso.compiler.core.ir.module.scope;
  exports org.enso.compiler.core.ir.module.scope.definition;
  exports org.enso.compiler.core.ir.module.scope.imports;
  exports org.enso.compiler.core.ir.type;
}
