package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;

import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

/** A representation of an Atom constructor for module. */
@ExportLibrary(InteropLibrary.class)
public class ModuleAtomConstructor extends AtomConstructor {

  /**
   * Creates a new Atom constructor for a given module. The constructor is not valid until {@link
   * AtomConstructor#initializeFields(ArgumentDefinition...)} is called.
   *
   * @param name the name of the Atom constructor.
   * @param definitionScope the scope in which this constructor was defined.
   */
  public ModuleAtomConstructor(String name, ModuleScope definitionScope) {
    super(name, definitionScope);
  }

  /** @return the fully qualified name of module constructor. */
  @Override
  public QualifiedName getQualifiedName() {
    return this.getDefinitionScope().getModule().getName();
  }
}
