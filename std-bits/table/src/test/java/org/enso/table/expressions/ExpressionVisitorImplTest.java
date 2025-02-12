import java.nio.file.Paths;

import static org.mockito.Mockito.*;

import java.util.function.Function;
import java.util.logging.Level;

import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.EnsoContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import org.enso.table.expressions.ExpressionVisitorImpl;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.io.IOAccess;

import static org.junit.Assert.assertEquals;

public class ExpressionVisitorImplTest {
    private Function<String, Value> getColumn;
    private Function<Object, Value> makeConstantColumn;
    private EnsoContext polyglotContext;  // GraalVM Context

    @SuppressWarnings("unchecked")
    @Before
    public void setUp() {
        Engine eng =
            Engine.newBuilder()
                .allowExperimentalOptions(true)
                .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
                .option(RuntimeOptions.STRICT_ERRORS, "false")
                .logHandler(System.err)
                .option(
                    RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                    Paths.get("../../distribution/component").toFile().getAbsolutePath())
                .build();
        this.polyglotContext = Context.newBuilder().engine(eng).allowIO(IOAccess.ALL).allowAllAccess(true).build();

        getColumn = (Function<String, Value>) mock(Function.class);
        makeConstantColumn = (Function<Object, Value>) mock(Function.class);
    }

    private Value evaluate(String expr) {
        return ExpressionVisitorImpl.evaluate(
            expr, getColumn, makeConstantColumn, "test_module", "test_type", new String[]{});
    }

    @Test
    public void testAddition() {
        Value result = evaluate("1 + 2");
        assertEquals(3, result.asInt());
    }
}
