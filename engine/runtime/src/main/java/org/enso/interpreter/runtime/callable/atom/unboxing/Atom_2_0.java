package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

public class Atom_2_0 extends UnboxingAtom {
    private long field0;
    private long field1;

    // fast path
    public Atom_2_0(AtomConstructor constructor, Layout layout, long field0, long field1) {
        super(constructor, layout);
        this.field0 = field0;
        this.field1 = field1;
    }

    public static NodeFactory<? extends FieldGetterNode> getFieldGetterNodeFactory(int storageIndex, boolean isDoubleIfUnboxed) {
        return switch (storageIndex) {
            case 0 -> isDoubleIfUnboxed ?
                    Atom_2_0Factory.FieldGetter_0D_NodeFactory.getInstance() :
                    Atom_2_0Factory.FieldGetter_0L_NodeFactory.getInstance();
            case 1 -> isDoubleIfUnboxed ?
                    Atom_2_0Factory.FieldGetter_1D_NodeFactory.getInstance() :
                    Atom_2_0Factory.FieldGetter_1L_NodeFactory.getInstance();
            default -> throw new IllegalArgumentException("Invalid storage index");
        };
    }

    public static abstract class FieldGetter_1D_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return Double.longBitsToDouble(atom.field1);
        }
    }

    public static abstract class FieldGetter_0L_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return atom.field0;
        }
    }

    public static abstract class FieldGetter_0D_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return Double.longBitsToDouble(atom.field0);
        }
    }

    public static abstract class FieldGetter_1L_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return atom.field1;
        }
    }

    public static abstract class InstantiatorNode extends UnboxingAtom.InstantiatorNode {
        @Specialization
        Atom doExecute(AtomConstructor constructor, Layout layout, Object[] args) {
            return new Atom_2_0(constructor, layout, (long) args[0], (long) args[1]);
        }
    }

    public static NodeFactory<Atom_2_0.InstantiatorNode> getInstantiatorNodeFactory() {
        return Atom_2_0Factory.InstantiatorNodeFactory.getInstance();
    }
}
