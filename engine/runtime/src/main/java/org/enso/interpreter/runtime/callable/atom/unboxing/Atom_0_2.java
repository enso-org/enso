package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

public class Atom_0_2 extends UnboxingAtom {
    private Object field0;
    private Object field1;

    public Atom_0_2(AtomConstructor constructor, Layout layout, Object field0, Object field1) {
        super(constructor, layout);
        this.field0 = field0;
        this.field1 = field1;
    }


    public static NodeFactory<? extends FieldGetterNode> getFieldGetterNodeFactory(int storageIndex, boolean isDoubleIfUnboxed) {
        return switch (storageIndex) {
            case 0 -> Atom_0_2Factory.FieldGetter_0_NodeFactory.getInstance();
            case 1 -> Atom_0_2Factory.FieldGetter_1_NodeFactory.getInstance();
            default -> throw new IllegalArgumentException("Invalid storage index");
        };
    }

    public abstract static class FieldGetter_0_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_0_2 atom) {
            return atom.field0;
        }
    }

    public abstract static class FieldGetter_1_Node extends FieldGetterNode {
        @Specialization
        Object doAtom(Atom_0_2 atom) {
            return atom.field1;
        }
    }

    public static abstract class InstantiatorNode extends UnboxingAtom.InstantiatorNode {
        @Specialization
        Atom doExecute(AtomConstructor constructor, Layout layout, Object[] args) {
            return new Atom_0_2(constructor, layout, args[0], args[1]);
        }
    }


    public static NodeFactory<Atom_0_2.InstantiatorNode> getInstantiatorNodeFactory() {
        return Atom_0_2Factory.InstantiatorNodeFactory.getInstance();
    }
}
