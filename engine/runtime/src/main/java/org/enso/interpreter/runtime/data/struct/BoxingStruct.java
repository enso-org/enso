package org.enso.interpreter.runtime.data.struct;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(StructsLibrary.class)
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public class BoxingStruct extends Struct {
    private final Object[] fields;

    public BoxingStruct(AtomConstructor constructor, Object... fields) {
        super(constructor);
        this.fields = fields;
    }

    @ExportMessage(name = "getFields")
    Object[] getFieldsX() {
        return fields;
    }

    @ExportMessage
    Object getField(int index) {
        return fields[index];
    }
}
