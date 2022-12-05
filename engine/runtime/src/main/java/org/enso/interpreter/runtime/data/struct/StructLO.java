package org.enso.interpreter.runtime.data.struct;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(StructsLibrary.class)
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public class StructLO extends Struct {
    private final long field0;
    private final Object field1;

    public StructLO(AtomConstructor constructor, long field0, Object field1) {
        super(constructor);
        this.field0 = field0;
        this.field1 = field1;
    }


    @ExportMessage(name = "getFields")
    Object[] getFieldsX() {
        return new Object[]{field0, field1};
    }

    @ExportMessage
    Object getField(int index) {
        switch (index) {
            case 0:
                return field0;
            case 1:
                return field1;
            default:
                return null;
        }
    }
}
