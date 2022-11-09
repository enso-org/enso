package org.enso.interpreter.runtime.data.struct;

public class BoxingStruct extends Struct {
    private final Object[] fields;

    public BoxingStruct(AtomConstructor constructor, Object... fields) {
        super(constructor);
        this.fields = fields;
    }
}
