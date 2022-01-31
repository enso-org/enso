package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Array", name = "clone", description = "Copies one array to another.")
public abstract class CloneNode extends Node {

    static CloneNode build() {
        return CloneNodeGen.create();
    }

    abstract Object execute(
        Object _this, Object src, long source_index, long source_count, long new_size);

    @Specialization
    Object doArray(
            Object _this,
            Array src,
            long source_index,
            long source_count,
            long new_size,
            @CachedContext(Language.class) Context ctx) {
        Array output = new Array(new_size);
        System.arraycopy(
                src.getItems(), (int) source_index, output.getItems(), 0, (int) source_count);
        return output;
    }

    @Specialization
    Object doPolyglotArray(
            Object _this,
            byte[] src,
            long source_index,
            long source_count,
            long new_size,
            @CachedContext(Language.class) Context ctx) {
        byte[] output = new byte[(int) new_size];
        System.arraycopy(src, (int) source_index, output, 0, (int) source_count);
        return output;
    }

    @Specialization
    Object doPolyglotArray(
            Object _this,
            int[] src,
            long source_index,
            long source_count,
            long new_size,
            @CachedContext(Language.class) Context ctx) {
        int[] output = new int[(int) new_size];
        System.arraycopy(src, (int) source_index, output, 0, (int) source_count);
        return output;
    }

    @Specialization
    Object doPolyglotArray(
            Object _this,
            long[] src,
            long source_index,
            long source_count,
            long new_size,
            @CachedContext(Language.class) Context ctx) {
        long[] output = new long[(int) new_size];
        System.arraycopy(src, (int) source_index, output, 0, (int) source_count);
        return output;
    }

    @Specialization
    Object doPolyglotArray(
            Object _this,
            char[] src,
            long source_index,
            long source_count,
            long new_size,
            @CachedContext(Language.class) Context ctx) {
        char[] output = new char[(int) new_size];
        System.arraycopy(src, (int) source_index, output, 0, (int) source_count);
        return output;
    }

    @Specialization
    Object doPolyglotArray(
            Object _this,
            float[] src,
            long source_index,
            long source_count,
            long new_size,
            @CachedContext(Language.class) Context ctx) {
        float[] output = new float[(int) new_size];
        System.arraycopy(src, (int) source_index, output, 0, (int) source_count);
        return output;
    }

    @Specialization
    Object doPolyglotArray(
            Object _this,
            double[] src,
            long source_index,
            long new_size,
            long source_count,
            @CachedContext(Language.class) Context ctx) {
        double[] output = new double[(int) new_size];
        System.arraycopy(src, (int) source_index, output, 0, (int) source_count);
        return output;
    }

    @Specialization(guards = "arrays.hasArrayElements(src)")
    Object doPolyglotArray(
            Object _this,
            Object src,
            long source_index,
            long new_size,
            long source_count,
            @CachedLibrary(limit = "3") InteropLibrary arrays,
            @CachedContext(Language.class) Context ctx) {
        Array dest = new Array(new_size);
        try {
            for (int i = 0; i < source_count; i++) {
                dest.getItems()[i] = arrays.readArrayElement(src, source_index + i);
            }
        } catch (UnsupportedMessageException e) {
            throw new IllegalStateException("Unreachable");
        } catch (InvalidArrayIndexException e) {
            throw new PanicException(
                    ctx.getBuiltins().error().makeInvalidArrayIndexError(src, e.getInvalidIndex()), this);
        }
        return dest;
    }

    @Fallback
    Object doOther(
            Object _this,
            Object src,
            long source_index,
            long source_count,
            long new_size) {
        Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
        throw new PanicException(
                builtins.error().makeTypeError(builtins.mutable().array().newInstance(), src, "src"), this);
    }
}