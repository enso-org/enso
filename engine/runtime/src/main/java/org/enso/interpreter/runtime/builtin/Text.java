package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.text.ConcatMethodGen;
import org.enso.interpreter.node.expression.builtin.text.OptimizeMethodGen;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all Text-related stdlib builtins. */
public class Text {
  private final AtomConstructor text;

  /**
   * Creates and registers all the text constructors and methods.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors and methods in.
   */
  public Text(Language language, ModuleScope scope) {
    text = new AtomConstructor("Text", scope).initializeFields();
    scope.registerConstructor(text);
    AtomConstructor primTextHelpers =
        new AtomConstructor("Prim_Text_Helper", scope).initializeFields();
    scope.registerConstructor(primTextHelpers);

    scope.registerMethod(text, "+", ConcatMethodGen.makeFunction(language));
    scope.registerMethod(primTextHelpers, "optimize", OptimizeMethodGen.makeFunction(language));
  }

  /** @return the Text atom constructor. */
  public AtomConstructor getText() {
    return text;
  }
}
