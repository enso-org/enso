package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.struct.Struct;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.struct.StructsLibrary;

@BuiltinMethod(
        type = "Meta",
        name = "get_atom_fields",
        description = "Gets the fields of an unresolved atom.",
        autoRegister = false)
public abstract class GetAtomFieldsNode extends Node {
    abstract Array execute(Struct struct);

    @Specialization
    Array doStruct(Struct struct, @CachedLibrary(limit = "2") StructsLibrary structs) {
        return new Array(structs.getFields(struct));
    }

}
