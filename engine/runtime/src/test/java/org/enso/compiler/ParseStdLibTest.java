package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.function.Function;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.enso.compiler.codegen.AstToIr;
import org.enso.compiler.core.IR;
import org.enso.syntax.text.AST.ASTOf;
import org.enso.syntax.text.Parser;
import org.enso.syntax.text.Shape;
import org.junit.runner.RunWith;
import org.junit.runners.AllTests;

@RunWith(AllTests.class)
public final class ParseStdLibTest extends TestCase {
    private static final EnsoCompiler ensoCompiler = new EnsoCompiler();
    private final File where;

    private ParseStdLibTest(String name, File where) {
        super(name);
        this.where = where;
    }

    public static TestSuite suite() throws Exception {
        TestSuite s = new TestSuite();
        collectDistribution(s, "Base");
        return s;
    }

    private static Path locateDistribution(final String name) throws URISyntaxException {
        var where = new File(ParseStdLibTest.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        var dir = where;
        for (;;) {
            dir = new File(new File(new File(new File(new File(new File(where, "distribution"), "lib"), "Standard"), name), "0.0.0-dev"), "src");
            if (dir.exists()) {
                break;
            }
            where = where.getParentFile();
        }
        return dir.toPath();
    }

    private static void collectDistribution(TestSuite s, String name) throws Exception {
        class CollectSuites implements FileVisitor<Path> {

            private final TestSuite suite;

            CollectSuites(TestSuite suite) {
                this.suite = suite;
            }

            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                if (!file.getFileName().toString().endsWith(".enso")) {
                    return FileVisitResult.CONTINUE;
                }
                final String name = file.getFileName().toString();
                if (isKnownToWork(name)) {
                    suite.addTest(new ParseStdLibTest(name, file.toFile()));
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                return FileVisitResult.CONTINUE;
            }
        }
        var dir = locateDistribution(name);
        Files.walkFileTree(dir, new CollectSuites(s));
    }

    @SuppressWarnings("unchecked")
    private void parseTest(Source src) throws IOException {
        var ir = ensoCompiler.compile(src);
        assertNotNull("IR was generated", ir);

        var oldAst = new Parser().runWithIds(src.getCharacters().toString());
        var oldIr = AstToIr.translate((ASTOf<Shape>) (Object) oldAst);

        Function<IR, String> filter = (i) -> {
            var txt = i.pretty().replaceAll("id = [0-9a-f\\-]*", "id = _");
            for (;;) {
                final String pref = "IdentifiedLocation(";
                int at = txt.indexOf(pref);
                if (at == -1) {
                    break;
                }
                int to = at + pref.length();
                int depth = 1;
                while (depth > 0) {
                    switch (txt.charAt(to)) {
                        case '(' ->
                            depth++;
                        case ')' ->
                            depth--;
                    }
                    to++;
                }
                txt = txt.substring(0, at) + "IdentifiedLocation[_]" + txt.substring(to);
            }
            return txt;
        };

        var old = filter.apply(oldIr);
        var now = filter.apply(ir);
        if (!old.equals(now)) {
            var name = getName();
            var result = new File(src.getURI()).getParentFile().toPath();
            final Path oldPath = result.resolve(name + ".old");
            Files.writeString(oldPath, old, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
            final Path nowPath = result.resolve(name + ".now");
            Files.writeString(nowPath, now, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
            fail("IR for " + src.getPath() + " shall be equal:\n$ diff -u '" + oldPath + "' '" + nowPath + "'");
        }
    }

    @Override
    public void runBare() throws Throwable {
        var code = Files.readString(where.toPath());
        var src = Source.newBuilder("enso", code, getName())
            .uri(where.toURI())
            .build();
        parseTest(src);
    }

    private static boolean isKnownToWork(String name) {
        switch (name) {
            case "Array.enso":
                return false;
            case "Body.enso":
                return false;
            case "Case.enso":
                return false;
            case "Case_Sensitivity.enso":
                return false;
            case "Default.enso":
                return false;
            case "Encoding.enso":
                return false;
            case "Engine.enso":
                return false;
            case "Enso_Project.enso":
                return false;
            case "Extensions.enso":
                return false;
            case "Form.enso":
                return false;
            case "Generator.enso":
                return false;
            case "Header.enso":
                return false;
            case "Http.enso":
                return false;
            case "Index_Sub_Range.enso":
                return false;
            case "Internal.enso":
                return false;
            case "Interval.enso":
                return false;
            case "Line_Ending_Style.enso":
                return false;
            case "Locale.enso":
                return false;
            case "Main.enso":
                return false;
            case "Map.enso":
                return false;
            case "Matching.enso":
                return false;
            case "Method.enso":
                return false;
            case "Natural_Order.enso":
                return false;
            case "Option.enso":
                return false;
            case "Ordering.enso":
                return false;
            case "Proxy.enso":
                return false;
            case "Proxy_Polyglot_Array.enso":
                return false;
            case "Range.enso":
                return false;
            case "Regex.enso":
                return false;
            case "Regex_Matcher.enso":
                return false;
            case "Regex_Mode.enso":
                return false;
            case "Request.enso":
                return false;
            case "Resource.enso":
                return false;
            case "Response.enso":
                return false;
            case "Runtime.enso":
                return false;
            case "Sort_Direction.enso":
                return false;
            case "Span.enso":
                return false;
            case "Statistics.enso":
                return false;
            case "Status_Code.enso":
                return false;
            case "System.enso":
                return false;
            case "Text_Matcher.enso":
                return false;
            case "Text_Ordering.enso":
                return false;
            case "Text_Sub_Range.enso":
                return false;
            case "URI.enso":
                return false;
            case "Vector_Lexicographic_Order.enso":
                return false;
            case "Version.enso":
                return false;
            case "Warning.enso":
                return false;
            case "Common.enso":
                return false;
            case "Date.enso":
                return false;
            case "Date_Period.enso":
                return false;
            case "Date_Time.enso":
                return false;
            case "Day_Of_Week.enso":
                return false;
            case "Day_Of_Week_From.enso":
                return false;
            case "Duration.enso":
                return false;
            case "Environment.enso":
                return false;
            case "Existing_File_Behavior.enso":
                return false;
            case "Exit_Code.enso":
                return false;
            case "File.enso":
                return false;
            case "File_Format.enso":
                return false;
            case "File_Permissions.enso":
                return false;
            case "Function.enso":
                return false;
            case "List.enso":
                return false;
            case "Maybe.enso":
                return false;
            case "Meta.enso":
                return false;
            case "Pair.enso":
                return false;
            case "Platform.enso":
                return false;
            case "Problem_Behavior.enso":
                return false;
            case "Process.enso":
                return false;
            case "Random.enso":
                return false;
            case "Time_Of_Day.enso":
                return false;
            case "Time_Zone.enso":
                return false;
            case "Numbers.enso":
                return false;
            case "Vector.enso":
                return false;
            case "Any.enso":
                return false;
            case "Filter_Condition.enso":
                return false;
            case "Json.enso":
                return false;
            case "Bound.enso":
                return false;
            case "Rank_Method.enso":
                return false;
            case "Regression.enso":
                return false;
        }
        return true;
    }
}
