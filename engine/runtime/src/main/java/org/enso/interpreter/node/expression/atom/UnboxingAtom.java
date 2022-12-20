package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

public abstract class UnboxingAtom extends Atom {
    private final Layout layout;

    public UnboxingAtom(AtomConstructor constructor, Layout layout) {
        super(constructor);
        this.layout = layout;
    }

    @GenerateNodeFactory
    @GenerateUncached
    public static abstract class FieldGetterNode extends Node {
        public abstract Object execute(Atom atom);
    }

    @GenerateNodeFactory
    @GenerateUncached
    public static abstract class NewInstanceNode extends Node {
        public abstract Atom execute(Object[] arguments);
    }

    public static class Layout {
        // this will work until 32 fields, then we need to fall back to all-unboxed
        private final long inputFlags;
        private final @CompilerDirectives.CompilationFinal(dimensions = 1) int[] fieldToStorage;
        private final @CompilerDirectives.CompilationFinal(dimensions = 1) NodeFactory<? extends FieldGetterNode>[] fieldGetterFactories;
        private final @CompilerDirectives.CompilationFinal(dimensions = 1) FieldGetterNode[] uncachedFieldGetters;

        public Layout(long inputFlags, int[] fieldToStorage, NodeFactory<? extends FieldGetterNode>[] fieldGetterFactories) {
            this.inputFlags = inputFlags;
            this.fieldToStorage = fieldToStorage;
            this.fieldGetterFactories = fieldGetterFactories;
            this.uncachedFieldGetters = new FieldGetterNode[fieldGetterFactories.length];
            for (int i = 0; i < fieldGetterFactories.length; i++) {
                this.uncachedFieldGetters[i] = fieldGetterFactories[i].getUncachedInstance();
            }
        }

        public static Layout create(int arity, long typeFlags) {
            if (arity > 32) {
                throw new IllegalArgumentException("Too many fields in unboxed atom");
            }
            int numUnboxed = Long.bitCount(typeFlags);
            int[] fieldToStorage = new int[arity];
            int lastUnboxed = 0;
            int lastBoxed = numUnboxed;
            for (int cur = 0; cur < arity; cur++) {
                if ((typeFlags & (0b11L << 2 * cur)) != 0) {
                    fieldToStorage[cur] = lastUnboxed++;
                } else {
                    fieldToStorage[cur] = lastBoxed++;
                }
            }

            var numBoxed = arity - numUnboxed;

            var getterFactories = new NodeFactory[arity];

            for (int i = 0; i < arity; i++) {
                var isDouble = (typeFlags & (0b01L << 2 * i)) != 0;
                getterFactories[i] = switch (numUnboxed) {
                    case 0 -> switch (numBoxed) {
                        case 2 -> Atom_0_2.getFieldGetterNodeFactory(fieldToStorage[i], isDouble);
                        default -> throw new IllegalArgumentException("Unsupported arity");
                    };
                    case 1 -> switch (numBoxed) {
                        case 1 -> Atom_1_1.getFieldGetterNodeFactory(fieldToStorage[i], isDouble);
                        default -> throw new IllegalArgumentException("Unsupported arity");
                    };
                    case 2 -> switch (numBoxed) {
                        case 0 -> Atom_2_0.getFieldGetterNodeFactory(fieldToStorage[i], isDouble);
                        default -> throw new IllegalArgumentException("Unsupported arity");
                    };
                    default -> throw new IllegalArgumentException("Unsupported arity");
                };
            }

            return new Layout(typeFlags, fieldToStorage, getterFactories);
        }

    }

    public abstract static class Atom_0_2_FieldGetter_0_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_0_2 atom) {
            return atom.field0;
        }
    }

    public abstract static class Atom_0_2_FieldGetter_1_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_0_2 atom) {
            return atom.field1;
        }
    }

    public static class Atom_0_2 extends UnboxingAtom {
        private Object field0;
        private Object field1;

        public Atom_0_2(AtomConstructor constructor, Layout layout, Object field0, Object field1) {
            super(constructor, layout);
            this.field0 = field0;
            this.field1 = field1;
        }


        public static NodeFactory<? extends FieldGetterNode> getFieldGetterNodeFactory(int storageIndex, boolean isDoubleIfUnboxed) {
            return switch (storageIndex) {
                case 0 -> UnboxingAtomFactory.Atom_0_2_FieldGetter_0_NodeFactory.getInstance();
                case 1 -> UnboxingAtomFactory.Atom_0_2_FieldGetter_1_NodeFactory.getInstance();
                default -> throw new IllegalArgumentException("Invalid storage index");
            };
        }
    }


    public static abstract class Atom_1_1_FieldGetter_0L_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_1_1 atom) {
            return atom.field0;
        }
    }

    public static abstract class Atom_1_1_FieldGetter_0D_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_1_1 atom) {
            return Double.longBitsToDouble(atom.field0);
        }
    }

    public static abstract class Atom_1_1_FieldGetter_1_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_1_1 atom) {
            return atom.field1;
        }
    }

    public static class Atom_1_1 extends UnboxingAtom {
        private long field0;
        private Object field1;

        // fast path
        public Atom_1_1(AtomConstructor constructor, Layout layout, long field0, Object field1) {
            super(constructor, layout);
            this.field0 = field0;
            this.field1 = field1;
        }

        public static NodeFactory<? extends FieldGetterNode> getFieldGetterNodeFactory(int storageIndex, boolean isDoubleIfUnboxed) {
            return switch (storageIndex) {
                case 0 -> isDoubleIfUnboxed ?
                        UnboxingAtomFactory.Atom_1_1_FieldGetter_0D_NodeFactory.getInstance() :
                        UnboxingAtomFactory.Atom_1_1_FieldGetter_0L_NodeFactory.getInstance();
                case 1 -> UnboxingAtomFactory.Atom_1_1_FieldGetter_1_NodeFactory.getInstance();
                default -> throw new IllegalArgumentException("Invalid storage index");
            };
        }
    }

    public static abstract class Atom_2_0_FieldGetter_0L_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return atom.field0;
        }
    }

    public static abstract class Atom_2_0_FieldGetter_0D_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return Double.longBitsToDouble(atom.field0);
        }
    }

    public static abstract class Atom_2_0_FieldGetter_1L_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return atom.field1;
        }
    }

    public static abstract class Atom_2_0_FieldGetter_1D_Node extends UnboxingAtom.FieldGetterNode {
        @Specialization
        Object doAtom(Atom_2_0 atom) {
            return Double.longBitsToDouble(atom.field1);
        }
    }

    public static class Atom_2_0 extends UnboxingAtom {
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
                        UnboxingAtomFactory.Atom_2_0_FieldGetter_0D_NodeFactory.getInstance() :
                        UnboxingAtomFactory.Atom_2_0_FieldGetter_0L_NodeFactory.getInstance();
                case 1 -> isDoubleIfUnboxed ?
                        UnboxingAtomFactory.Atom_2_0_FieldGetter_1D_NodeFactory.getInstance() :
                        UnboxingAtomFactory.Atom_2_0_FieldGetter_1L_NodeFactory.getInstance();
                default -> throw new IllegalArgumentException("Invalid storage index");
            };
        }
    }


    static class ComputeDigest2Node extends Node {

    }

    abstract static class CreateLayoutInstanceNode extends Node {

    }

    abstract static class CreateInstance2Node extends InstantiateNode.CreateInstanceNode {
        @Children
        CreateLayoutInstanceNode[] layouts;

        final AtomConstructor constructor;

        CreateInstance2Node(AtomConstructor constructor) {
            this.constructor = constructor;
        }

        @Specialization
        Object doExecute(Object[] arguments) {
            return constructor.newInstance(arguments);
        }
    }
}
