package org.enso.compiler.dump.igv;

import java.io.File;
import java.io.IOException;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumper;
import org.graalvm.graphio.GraphOutput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IGVDumper implements IRDumper {

  private static final String DEFAULT_DUMP_DIR = "ir-dumps";
  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumper.class);
  private final String moduleName;
  private final Path outPath;
  private final GraphOutput<EnsoModuleAST, ASTMethod> graphOutput;
  private final ExecutorService executor;
  private final ConcurrentLinkedQueue<CompletableFuture<Void>> tasks =
      new ConcurrentLinkedQueue<>();
  private int currGraphId;
  private boolean groupCreated;

  /** Count of all the nodes for all the subgraphs */
  private int nodesCnt;

  private IGVDumper(
      String moduleName,
      Path outPath,
      GraphOutput<EnsoModuleAST, ASTMethod> graphOutput,
      ExecutorService executor) {
    this.moduleName = moduleName;
    this.outPath = outPath;
    this.graphOutput = graphOutput;
    this.executor = executor;
  }

  static IGVDumper createForModule(String moduleName, ExecutorService executor) {
    var outPath = outputForModule(moduleName);
    var channel = createFileChannel(outPath);
    GraphOutput<EnsoModuleAST, ASTMethod> graphOutput;
    try {
      graphOutput =
          GraphOutput.newBuilder(EnsoModuleAST.AST_DUMP_STRUCTURE)
              .blocks(EnsoModuleAST.AST_DUMP_STRUCTURE)
              .elementsAndLocations(
                  EnsoModuleAST.AST_DUMP_STRUCTURE, EnsoModuleAST.AST_DUMP_STRUCTURE)
              .build(channel);
    } catch (IOException e) {
      LOGGER.error("Failed to create graph output for module {}", moduleName, e);
      return null;
    }
    return new IGVDumper(moduleName, outPath, graphOutput, executor);
  }

  @Override
  public void dump(Module ir, String moduleName, File srcFile, String afterPass) {
    assert moduleName.equals(this.moduleName);
    var task =
        CompletableFuture.runAsync(() -> dumpTask(ir, moduleName, srcFile, afterPass), executor);
    tasks.add(task);
  }

  private void dumpTask(Module ir, String moduleName, File srcFile, String afterPass) {
    LOGGER.trace(
        "[{}] Creating EnsoModuleAST after pass {}, nodeId = {}", moduleName, afterPass, nodesCnt);
    var moduleAst = EnsoModuleAST.fromIR(ir, srcFile, moduleName, nodesCnt);
    nodesCnt += moduleAst.getNodes().size();
    try {
      if (!groupCreated) {
        graphOutput.beginGroup(moduleAst, moduleName, moduleName, null, 0, null);
        groupCreated = true;
      }
      var props = new HashMap<>();
      LOGGER.trace("[{}] Printing module AST with ID {}", moduleName, currGraphId);
      graphOutput.print(moduleAst, props, currGraphId, "%s", afterPass);
    } catch (IOException e) {
      LOGGER.error("[{}] Failed to dump the graph for pass {}", moduleName, afterPass);
      throw new RuntimeException(e);
    }
    currGraphId++;
    LOGGER.trace("[{}] Dumped after pass {}", moduleName, afterPass);
  }

  @Override
  public void close() {
    var tasksArr = tasks.toArray(CompletableFuture[]::new);
    var allTasks = CompletableFuture.allOf(tasksArr);
    try {
      allTasks.get();
    } catch (InterruptedException | ExecutionException e) {
      LOGGER.error("Failed to wait for all tasks to complete", e);
    }
    try {
      graphOutput.endGroup();
    } catch (IOException e) {
      LOGGER.error("[%s] Failed to end the group".formatted(moduleName), e);
      return;
    }
    graphOutput.close();
    LOGGER.trace("[{}] Graph dumped to {}", moduleName, outPath);
  }

  private static Path outputForModule(String moduleName) {
    var irDumpsDir = Path.of(DEFAULT_DUMP_DIR);
    if (!irDumpsDir.toFile().exists()) {
      try {
        Files.createDirectory(irDumpsDir);
      } catch (IOException e) {
        throw new IllegalStateException(e);
      }
    }
    var outPath = irDumpsDir.resolve(moduleName + ".bgv");
    if (!outPath.toFile().exists()) {
      try {
        Files.createFile(outPath);
      } catch (IOException e) {
        LOGGER.error("Failed to create output: {}", outPath, e);
      }
    }
    return outPath;
  }

  private static WritableByteChannel createFileChannel(Path path) {
    try {
      return Files.newByteChannel(
          path, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  private record PassGraph(String passName, EnsoModuleAST ast) {}
}
