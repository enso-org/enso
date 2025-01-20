package org.enso.compiler.dump.igv;

import java.io.IOException;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import org.graalvm.graphio.GraphOutput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * For a single module, we want to see IRs for all the passes. This class wraps a module with ASTs
 * for every pass.
 */
public final class ModuleGraph {
  private final String moduleName;
  private final List<PassGraph> passGraphs = new ArrayList<>();
  private static final Logger LOGGER = LoggerFactory.getLogger(ModuleGraph.class);

  public ModuleGraph(String moduleName) {
    this.moduleName = moduleName;
  }

  public String getModuleName() {
    return moduleName;
  }

  public void addSubGraphForPass(String passName, EnsoModuleAST ast) {
    var containsGraphForPass = passGraphs.stream().anyMatch(g -> g.passName.equals(passName));
    if (containsGraphForPass) {
      LOGGER.warn("[{}] Pass graph already exists for {}", moduleName, passName);
    } else {
      LOGGER.trace("[{}] Adding pass graph for {}", moduleName, passName);
      passGraphs.add(new PassGraph(passName, ast));
    }
  }

  public List<ASTNode> getAllNodes() {
    var allNodes = passGraphs.stream().flatMap(passGraph -> passGraph.ast.getNodes().stream());
    return allNodes.toList();
  }

  /**
   * Dump all the module graphs into single output.
   *
   * @param outPath Output to dump to. Will be created if not exist. Not null.
   */
  public void dump(Path outPath) throws IOException {
    if (passGraphs.isEmpty()) {
      LOGGER.warn("[{}] No passes were added - not dumping anything", moduleName);
      return;
    }
    if (!outPath.toFile().exists()) {
      Files.createFile(outPath);
    }
    try (var channel = createFileChannel(outPath)) {
      var output =
          GraphOutput.newBuilder(EnsoModuleAST.AST_DUMP_STRUCTURE)
              .blocks(EnsoModuleAST.AST_DUMP_STRUCTURE)
              .elementsAndLocations(
                  EnsoModuleAST.AST_DUMP_STRUCTURE, EnsoModuleAST.AST_DUMP_STRUCTURE)
              .build(channel);
      int currGraphId = 0;
      boolean groupCreated = false;
      var props = new HashMap<>();
      for (var passGraph : passGraphs) {
        if (!groupCreated) {
          output.beginGroup(passGraph.ast, moduleName, moduleName, null, 0, null);
          groupCreated = true;
        }
        output.print(passGraph.ast, props, currGraphId, "%s", passGraph.passName);
        currGraphId++;
      }
      output.endGroup();
      output.close();
    }
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
