package org.enso.interpreter.runtime.callable.atom.unboxing;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.atom.InstantiateNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;

@ExportLibrary(StructsLibrary.class)
public abstract class UnboxingAtom extends Atom {
    protected final Layout layout;

    public UnboxingAtom(AtomConstructor constructor, Layout layout) {
        super(constructor);
        this.layout = layout;
    }

    @ExportMessage
    Object getField(int i) {
        return null;
    }

    @ExportMessage(name = "getFields")
    static class GetFields {
        @Specialization(guards = "cachedLayout == atom.layout", limit = "10")
        @ExplodeLoop
        static Object[] doCached(UnboxingAtom atom, @Cached("atom.layout") Layout cachedLayout, @Cached(value = "cachedLayout.buildGetters()") FieldGetterNode[] getters) {
            Object[] result = new Object[getters.length];
            for (int i = 0; i < getters.length; i++) {
                result[i] = getters[i].execute(atom);
            }
            return result;
        }

        @Specialization(replaces = "doCached")
        static Object[] doUncached(UnboxingAtom atom) {
            var getters = atom.layout.getUncachedFieldGetters();
            var result = new Object[getters.length];
            for (int i = 0; i < getters.length; i++) {
                result[i] = getters[i].execute(atom);
            }
            return result;
        }

    }

    @GenerateNodeFactory
    @GenerateUncached
    public static abstract class FieldGetterNode extends Node {
        public abstract Object execute(Atom atom);
    }

    @GenerateNodeFactory
    @GenerateUncached
    static abstract class InstantiatorNode extends Node {
        public abstract Atom execute(AtomConstructor constructor, Layout layout, Object[] args);
    }

    public static class CreateUnboxedInstanceNode extends InstantiateNode.CreateInstanceNode {
        @Child
        Layout.DirectCreateLayoutInstanceNode boxedLayout;
        @Children
        Layout.DirectCreateLayoutInstanceNode[] unboxedLayouts;
        private final int arity;
        private @CompilerDirectives.CompilationFinal boolean constructorAtCapacity;

        private final AtomConstructor constructor;

        CreateUnboxedInstanceNode(AtomConstructor constructor) {
            this.constructor = constructor;
            this.arity = constructor.getArity();
            this.boxedLayout = new Layout.DirectCreateLayoutInstanceNode(constructor, constructor.getBoxedLayout());
            unboxedLayouts = new Layout.DirectCreateLayoutInstanceNode[0];
            updateFromConstructor();
        }

        public static CreateUnboxedInstanceNode create(AtomConstructor constructor) {
            return new CreateUnboxedInstanceNode(constructor);
        }

        @Override
        @ExplodeLoop
        public Object execute(Object[] arguments) {
            var flags = computeFlags(arguments);
            if (flags == 0) {
                return boxedLayout.execute(arguments);
            }
            for (int i = 0; i < unboxedLayouts.length; i++) {
                if (unboxedLayouts[i].layout.inputFlags == flags) {
                    return unboxedLayouts[i].execute(arguments);
                }
            }

            if (!constructorAtCapacity) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                var lock = constructor.getLayoutsLock();
                lock.lock();
                try {
                    var layouts = constructor.getUnboxingLayouts();
                    if (layouts.length != this.unboxedLayouts.length) {
                        // Layouts changed since we last tried; Update & try again
                        updateFromConstructor();
                        return execute(arguments);
                    }

                    // Layouts didn't change; just create a new one and register it
                    var newLayout = Layout.create(arity, flags);
                    constructor.addLayout(newLayout);
                    updateFromConstructor();
                    return unboxedLayouts[unboxedLayouts.length - 1].execute(arguments);
                } finally {
                    lock.unlock();
                }
            }

            return boxedLayout.execute(arguments);
        }

        void updateFromConstructor() {
            var layouts = constructor.getUnboxingLayouts();
            var newLayouts = new Layout.DirectCreateLayoutInstanceNode[layouts.length];
            System.arraycopy(unboxedLayouts, 0, newLayouts, 0, unboxedLayouts.length);
            for (int i = unboxedLayouts.length; i < newLayouts.length; i++) {
                newLayouts[i] = new Layout.DirectCreateLayoutInstanceNode(constructor, layouts[i]);
            }
            if (layouts.length == Context.get(this).getMaxUnboxingLayouts()) {
                constructorAtCapacity = true;
            }
            unboxedLayouts = newLayouts;
        }

        @ExplodeLoop
        long computeFlags(Object[] arguments) {
            long flags = 0;
            for (int i = 0; i < arity; i++) {
                if (arguments[i] instanceof Double) {
                    flags |= Layout.DOUBLE_MASK << (i * 2);
                } else if (arguments[i] instanceof Long) {
                    flags |= Layout.LONG_MASK << (i * 2);
                }
            }
            return flags;
        }
    }
}
