package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

public class Atom_1_1 extends UnboxingAtom {
    private long field0;
    private Object field1;

    public Atom_1_1(AtomConstructor constructor, Layout layout, long field0, Object field1) {
        super(constructor, layout);
        this.field0 = field0;
        this.field1 = field1;
    }

    public static NodeFactory<? extends FieldGetterNode> getFieldGetterNodeFactory(int storageIndex, boolean isDoubleIfUnboxed) {
        return switch (storageIndex) {
            case 0 -> isDoubleIfUnboxed ?
                    Atom_1_1Factory.FieldGetter_0D_NodeFactory.getInstance() :
                    Atom_1_1Factory.FieldGetter_0L_NodeFactory.getInstance();
            case 1 -> Atom_1_1Factory.FieldGetter_1_NodeFactory.getInstance();
            default -> throw new IllegalArgumentException("Invalid storage index");
        };
    }

    public static abstract class FieldGetter_0L_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_1_1 atom) {
            return atom.field0;
        }
    }

    public static abstract class FieldGetter_0D_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_1_1 atom) {
            return Double.longBitsToDouble(atom.field0);
        }
    }

    public static abstract class FieldGetter_1_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_1_1 atom) {
            return atom.field1;
        }
    }

    public abstract static class InstantiatorNode extends UnboxingAtom.InstantiatorNode {
        @Specialization
        Atom doExecute(AtomConstructor constructor, Layout layout, Object[] args) {
            return new Atom_1_1(constructor, layout, (long) args[0], args[1]);
        }
    }

    public static NodeFactory<InstantiatorNode> getInstantiatorNodeFactory() {
        return Atom_1_1Factory.InstantiatorNodeFactory.getInstance();
    }
}
