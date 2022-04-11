package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import org.enso.compiler.exception.CompilerError;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A representation of an Atom constructor for BuiltinType. */
public class BuiltinAtomConstructor extends AtomConstructor {

    private ModuleScope shadowDefinitions;
    /**
     * Creates a new Atom constructor for a given name. The constructor is not valid until {@link
     * AtomConstructor#initializeFields(LocalScope, ExpressionNode[], ExpressionNode[], ArgumentDefinition...)} is called.
     *
     * @param name            the name of the Atom constructor
     * @param definitionScope the scope in which this constructor was defined
     */
    public BuiltinAtomConstructor(String name, ModuleScope definitionScope) {
        super(name, definitionScope);
    }

    public void setShadowDefinitions(ModuleScope scope) {
        if (shadowDefinitions != null) {
            throw new CompilerError("cannot set shadow definitions twice");
        }
        this.shadowDefinitions = scope;
    }

    public ModuleScope getShadowDefinitions() {
        return this.shadowDefinitions;
    }

    public BuiltinAtomConstructor initializeFields(ArgumentDefinition... args) {
      return (BuiltinAtomConstructor)super.initializeFields(args);
    }
}
