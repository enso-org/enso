package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.warning.AttachWarningMethodGen;
import org.enso.interpreter.node.expression.builtin.warning.GetValueMethodGen;
import org.enso.interpreter.node.expression.builtin.warning.GetWarningsMethodGen;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

public class Warning {
  public static void initWarningMethods(Language language, ModuleScope scope) {
    var warning = new AtomConstructor("Prim_Warning", scope).initializeFields();

    scope.registerConstructor(warning);
    scope.registerMethod(warning, "get_all", GetWarningsMethodGen.makeFunction(language));
    scope.registerMethod(warning, "attach", AttachWarningMethodGen.makeFunction(language));
    scope.registerMethod(warning, "get_value", GetValueMethodGen.makeFunction(language));
  }
}
