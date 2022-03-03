package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Ordering;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
        type = "Boolean",
        name = "compare_to",
        description = "Comparison for Booleans.")
public abstract class CompareToNode extends Node {
    static CompareToNode build() { return CompareToNodeGen.create(); }

    abstract Atom execute(Boolean _this, Object that);

    @Specialization
    Atom doBoolean(
            Boolean _this,
            Boolean that,
            @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctxRef,
            @Cached("getOrdering(ctxRef)") Ordering ordering) {
        if (_this == that) {
            return ordering.newEqual();
        } else if (_this) {
            return ordering.newGreater();
        } else {
            return ordering.newLess();
        }
    }

    @Specialization
    Atom doOther(
            Boolean _this, Object that, @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctxRef) {
        CompilerDirectives.transferToInterpreter();
        var bool = ctxRef.get().getBuiltins().bool().getBool().newInstance();
        var typeError = ctxRef.get().getBuiltins().error().makeTypeError(that, bool, "that");
        throw new PanicException(typeError, this);
    }

    Ordering getOrdering(TruffleLanguage.ContextReference<Context> ctxRef) {
        return ctxRef.get().getBuiltins().ordering();
    }
}