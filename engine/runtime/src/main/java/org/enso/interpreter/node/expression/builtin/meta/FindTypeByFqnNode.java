package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.pkg.QualifiedName;

@BuiltinMethod(
    type = "Meta",
    name = "find_type_by_qualified_name",
    description = "Finds a type by fully qualified name.",
    autoRegister = false)
public class FindTypeByFqnNode extends Node {
  @TruffleBoundary
  Object execute(Text fqn) {
    var ctx = EnsoContext.get(this);
    var fullName = QualifiedName.fromString(fqn.toString());
    if (fullName.getParent().isDefined()) {
      var moduleName = fullName.getParent().get();
      var maybeModule = ctx.getTopScope().getModule(moduleName.toString());
      if (maybeModule.isPresent()) {
        var foundType = maybeModule.get().getScope().getType(fullName.item());
        if (foundType.isPresent()) {
          return foundType.get();
        }
      }
    }
    var maybeModule = ctx.getTopScope().getModule(fullName.toString());
    if (maybeModule.isPresent()) {
      var foundType = maybeModule.get().getScope().getAssociatedType();
      if (foundType != null) {
        return foundType;
      }
    }
    return ctx.getBuiltins().error().makeModuleDoesNotExistError(fqn.toString());
  }
}
