package org.enso.checkparser;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import org.enso.compiler.EnsoCompiler;
import org.enso.compiler.codegen.AstToIr;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Comment$Documentation;
import org.enso.compiler.core.IR$Module$Scope$Definition;
import org.enso.compiler.core.IR$Type$Ascription;
import org.enso.syntax.text.AST;
import org.enso.syntax.text.Shape;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;
import org.graalvm.polyglot.Source;
import scala.Function1;
import scala.collection.immutable.List;

class LoadParser implements FileVisitor<Path>, AutoCloseable {
    private final File root;
    private final Parser parser;
    private final EnsoCompiler compiler;
    private final Set<Path> visited = new LinkedHashSet<>();
    private final Map<Path,Exception> failed = new LinkedHashMap<>();
    private final Set<Path> irTested = new LinkedHashSet<>();
    private final Map<Path,Exception> irFailed = new LinkedHashMap<>();
    private final Set<Path> irDiff = new LinkedHashSet<>();

    private LoadParser(File root) {
        this.parser = Parser.create();
        this.compiler = new EnsoCompiler();
        this.root = root;
    }

    @Override
    public void close() throws Exception {
        parser.close();
    }

    public static void main(String[] args) throws Exception {
        var root = new File(".").getAbsoluteFile();
        try (LoadParser checker = new LoadParser(root)) {
            checker.scan("distribution");
            checker.scan("test");

            checker.printSummary(true);
        }
    }

    private void scan(String path) throws IOException {
        var dir = root.toPath().resolve(path);
        assert Files.isDirectory(dir) : "isDirectory: " + dir;

        Files.walkFileTree(dir, this);
    }

    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
        return FileVisitResult.CONTINUE;
    }

    private static Exception newExceptionNoStack(String msg) {
        var ex = new Exception(msg);
        ex.setStackTrace(new StackTraceElement[0]);
        return ex;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        if (!file.getFileName().toString().endsWith(".enso")) {
            return FileVisitResult.CONTINUE;
        }
        visited.add(file);

        System.err.println("processing " + file);
        Source src = Source.newBuilder("enso", file.toFile()).build();
        TEST: try {
            Tree tree = parser.parse(src.getCharacters().toString());
            if (tree == null) {
                failed.put(file, newExceptionNoStack("Rust failed"));
            } else {
                IR.Module ir;
                try {
                    irTested.add(file);
                    IR.Module m = compiler.generateIR(tree);
                    if (m == null) {
                        throw new NullPointerException();
                    }
                    ir = sanitize(m);
                } catch (Exception ex) {
                    if (ex.getClass().getName().contains("UnhandledEntity")) {
                        if (ex.getMessage().contains("= Invalid[")) {
                            failed.put(file, newExceptionNoStack("Rust produces Invalid AST"));
                            break TEST;
                        }
                        if (ex.getMessage().contains("translateCaseBranch = Case[null, null")) {
                            failed.put(file, newExceptionNoStack("Rust provides null case"));
                            break TEST;
                        }
                    }
                    irFailed.put(file, ex);
                    break TEST;
                }

                var oldAst = new org.enso.syntax.text.Parser().runWithIds(src.getCharacters().toString());
                var oldIr = sanitize(AstToIr.translate((AST.ASTOf<Shape>)(Object)oldAst));

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
                        case '(': depth++; break;
                        case ')': depth--; break;
                      }
                      to++;
                    }
                    txt = txt.substring(0, at) + "IdentifiedLocation[_]" + txt.substring(to);
                  }
                  var sb = new StringBuilder();
                  for (String l : txt.split("\n")) {
                    final String pref = "IR.Comment.Documentation";
                    if (l.contains(pref)) {
                        continue;
                    }
                    sb.append(l).append("\n");
                  }
                  return sb.toString();
                };

                var old = filter.apply(oldIr);
                var now = filter.apply(ir);
                if (!old.equals(now)) {
                    irDiff.add(file);
                    var oldFile = file.getParent().resolve(file.getFileName() + ".old");
                    var nowFile = file.getParent().resolve(file.getFileName() + ".now");
                    System.err.println("difference1: " + oldFile);
                    System.err.println("difference2: " + nowFile);
                    Files.writeString(oldFile , old, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                    Files.writeString(nowFile, now, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                }
            }
        } catch (Exception ex) {
            failed.put(file, ex);
            System.err.println("failed " + file);
        }

        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        visited.add(file);
        failed.put(file, exc);
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
        return FileVisitResult.CONTINUE;
    }

    private void printSummary(boolean verbose) {
        if (verbose) {
            for (var en : failed.entrySet()) {
                var key = en.getKey();
                var value = en.getValue();
                System.err.println("Problem " + key);
                value.printStackTrace();
            }
            for (var en : irFailed.entrySet()) {
                var key = en.getKey();
                var value = en.getValue();
                System.err.println("File " + key);
                value.printStackTrace();
            }
        }
        System.out.println("Found " + visited.size() + " files. " + failed.size() + " failed to parse");
        System.out.println("From " + irTested.size() + " files " + irFailed.size() + " failed to produce IR");
        System.out.println("From " + (irTested.size() - irFailed.size()) + " files " + irDiff.size() + " have different IR");
    }

    private static IR.Module sanitize(IR.Module m) {
        class NoComments implements Function1<IR.Expression, IR.Expression> {
            @Override
            public IR.Expression apply(IR.Expression exp) {
                if (exp == null) {
                    return null;
                }
                if (exp instanceof IR$Comment$Documentation) {
                    return null;
                }
                return exp.mapExpressions(this);
            }
        }
        class NoCommentsInBindings implements Function1<IR$Module$Scope$Definition, Boolean> {
            @Override
            public Boolean apply(IR$Module$Scope$Definition exp) {
                if (exp instanceof IR$Comment$Documentation) {
                    return false;
                } else if (exp instanceof IR$Type$Ascription) {
                    return false;
                } else {
                    return true;
                }
            }
        }
        var m1 = m.mapExpressions(new NoComments());
        var m2 = m1.copy(
          m1.copy$default$1(),
          m1.copy$default$2(),
          (List<IR$Module$Scope$Definition>) m1.bindings().filter(new NoCommentsInBindings()),
          m1.copy$default$4(),
          m1.copy$default$5(),
          m1.copy$default$6(),
          m1.copy$default$7()
        );
        return m2;
    }
}
