package org.enso.interpreter.runtime.data.atom;

import org.enso.compiler.context.LocalScope;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Abstract base class for all nodes that access field(s) of an Atom. */
abstract class GetFieldBaseNode extends EnsoRootNode {
  protected final Type type;
  protected final String fieldName;
  protected final ModuleScope moduleScope;

  GetFieldBaseNode(EnsoLanguage language, Type type, String fieldName, ModuleScope moduleScope) {
    super(language, LocalScope.empty(), moduleScope, fieldName, null);
    this.type = type;
    this.fieldName = fieldName;
    this.moduleScope = moduleScope;
  }

  @Override
  public String getName() {
    return fieldName;
  }

  @Override
  public String getQualifiedName() {
    return type.getQualifiedName().createChild(fieldName).toString();
  }

  protected PanicException noSuchFieldPanic(Atom atom) {
    var nameText = Text.create(fieldName);
    return new PanicException(
        EnsoContext.get(this)
            .getBuiltins()
            .error()
            .getNoSuchFieldError()
            .newInstance(atom, nameText),
        this);
  }
}
