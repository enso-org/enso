package org.enso.interpreter.node.expression.builtin.meta;

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
  Object execute(Text fqn) {
    var ctx = EnsoContext.get(this);
    var fullName = QualifiedName.fromString(fqn.toString());
    if (fullName.getParent().isDefined()) {
      var moduleName = fullName.getParent().get();
      for (var m : ctx.getTopScope().getModules()) {
        if (m.getName().equals(moduleName)) {
          var foundType = m.getScope().getType(fullName.item());
          if (foundType.isPresent()) {
            return foundType.get();
          }
        }
      }
    }
    return ctx.getBuiltins().error().makeModuleDoesNotExistError(fqn);
  }
}
