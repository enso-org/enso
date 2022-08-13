package org.enso.checkparser;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import org.enso.compiler.EnsoCompiler;
import org.enso.compiler.core.IR;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;
import org.enso.syntax2.UnsupportedSyntaxException;
import org.graalvm.polyglot.Source;

class LoadParser implements FileVisitor<Path>, AutoCloseable {
    private final File root;
    private final Parser parser;
    private final EnsoCompiler compiler;
    private final Set<Path> visited = new LinkedHashSet<>();
    private final Set<Path> failed = new LinkedHashSet<>();
    private final Set<Path> irTested = new LinkedHashSet<>();
    private final Map<Path,Exception> irFailed = new LinkedHashMap<>();

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
            checker.scan("tests");

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

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        if (!file.getFileName().toString().endsWith(".enso")) {
            return FileVisitResult.CONTINUE;
        }
        visited.add(file);

        System.err.println("processing " + file);
        Source src = Source.newBuilder("enso", file.toFile()).build();
        try {
            Tree tree = parser.parse(src.getCharacters().toString());
            if (tree == null) {
                failed.add(file);
            } else {
                if (skipIrTest(file)) {
                    return FileVisitResult.CONTINUE;
                }
                irTested.add(file);
                try {
                    IR.Module m = compiler.generateIR(tree);
                    if (m == null) {
                        throw new NullPointerException();
                    }
                } catch (Exception ex) {
                    irFailed.put(file, ex);
                }
            }
        } catch (UnsupportedSyntaxException ex) {
            failed.add(file);
        }

        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        visited.add(file);
        failed.add(file);
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
        return FileVisitResult.CONTINUE;
    }

    private void printSummary(boolean verbose) {
        if (verbose) {
            for (var en : irFailed.entrySet()) {
                var key = en.getKey();
                var value = en.getValue();
                System.err.println("File " + key);
                value.printStackTrace();
            }
        }
        System.out.println("Found " + visited.size() + " files. " + failed.size() + " failed to parse");
        System.out.println("From " + irTested.size() + " files " + irFailed.size() + " failed to produce IR");
    }

    private static boolean skipIrTest(Path file) throws IOException {
        var caseOf = Pattern.compile("case.*of");
        for (var line : Files.readAllLines(file)) {
            if (caseOf.matcher(line).find()) {
                return true;
            }
        }
        return false;
    }
}
