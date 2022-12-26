package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;

@GenerateLibrary
public abstract class StructsLibrary extends Library {
    private static final LibraryFactory<StructsLibrary> FACTORY =
            LibraryFactory.resolve(StructsLibrary.class);

    public static LibraryFactory<StructsLibrary> getFactory() {
        return FACTORY;
    }

    public static StructsLibrary getUncached() {
        return FACTORY.getUncached();
    }

    public boolean isStruct(Object receiver) {
        return false;
    }

    public abstract Object[] getFields(Object receiver);

    public abstract AtomConstructor getConstructor(Object receiver);

    // TODO Delete, use specialized nodes instead, `index` is always compilation constant currently.
    public abstract Object getField(Object receiver, int index);
}
