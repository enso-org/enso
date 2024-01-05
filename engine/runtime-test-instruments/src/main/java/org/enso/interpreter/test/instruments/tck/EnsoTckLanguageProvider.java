package org.enso.interpreter.test.instruments.tck;

import java.util.Collection;
import java.util.List;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.tck.LanguageProvider;
import org.graalvm.polyglot.tck.Snippet;
import org.graalvm.polyglot.tck.TypeDescriptor;

public class EnsoTckLanguageProvider implements LanguageProvider {
    public EnsoTckLanguageProvider() {
    }

    @Override
    public String getId() {
        return "enso";
    }

    @Override
    public Value createIdentityFunction(Context context) {
        var id = context.eval("enso", """
        id a = a
        """).invokeMember("eval_expression", "id");
        return id;
    }

    @Override
    public Collection<? extends Snippet> createValueConstructors(Context context) {
        return List.of();
    }

    @Override
    public Collection<? extends Snippet> createExpressions(Context context) {
        var plus = context.eval("enso", """
        plus a b = a + b
        """).invokeMember("eval_expression", "plus");

        return List.of(
            Snippet.newBuilder("plus", plus, TypeDescriptor.executable(TypeDescriptor.NUMBER, TypeDescriptor.NUMBER, TypeDescriptor.NUMBER)).build()
        );
    }

    @Override
    public Collection<? extends Snippet> createStatements(Context context) {
        var when = context.eval("enso", """
        when c = if c then 1 else -1
        """).invokeMember("eval_expression", "when");;
        return List.of(
            Snippet.newBuilder("if", when, TypeDescriptor.NUMBER)
            .parameterTypes(TypeDescriptor.BOOLEAN)
            .build()
        );
    }

    @Override
    public Collection<? extends Snippet> createScripts(Context context) {
        return List.of();
    }

    @Override
    public Collection<? extends Source> createInvalidSyntaxScripts(Context context) {
        return List.of(
            Source.newBuilder("enso", """
            main = x + 2
            """, "unknown_x.enso").buildLiteral()
        );
    }
}
