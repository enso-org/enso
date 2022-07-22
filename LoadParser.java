package org.enso.syntax2;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.LinkedHashSet;
import java.util.Set;
import org.enso.syntax2.Parser;
import org.graalvm.polyglot.Source;

class LoadParser implements FileVisitor<Path>, AutoCloseable {
    private final File root;
    private final Parser parser;
    private final Set<Path> visited = new LinkedHashSet<>();
    private final Set<Path> failed = new LinkedHashSet<>();

    private LoadParser(File root) {
        this.parser = Parser.create();
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

            checker.printSummary(false);
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
        Tree text = parser.parse(src.getCharacters().toString());
        if (text == null) {
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
        System.out.println("Found " + visited.size() + " files. " + failed.size() + " failed to parse");
    }
}
