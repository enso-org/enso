package org.enso.interpreter.test.instruments.tck;

import java.util.Collection;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.tck.LanguageProvider;
import org.graalvm.polyglot.tck.Snippet;

public class EnsoTckLanguageProvider implements LanguageProvider {
    public EnsoTckLanguageProvider() {
    }

    @Override
    public String getId() {
        return "enso";
    }

    @Override
    public Value createIdentityFunction(Context context) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Collection<? extends Snippet> createValueConstructors(Context context) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Collection<? extends Snippet> createExpressions(Context context) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Collection<? extends Snippet> createStatements(Context context) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Collection<? extends Snippet> createScripts(Context context) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Collection<? extends Source> createInvalidSyntaxScripts(Context context) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}