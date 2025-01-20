package org.enso.compiler.dump.igv;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IGVDumper implements IRDumper {

  private static final String DEFAULT_DUMP_DIR = "ir-dumps";
  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumper.class);
  private final Map<String, ModuleGraph> moduleGraphs = new HashMap<>();

  @Override
  public void dump(Module ir, String moduleName, File srcFile, String afterPass) {
    var moduleGraph = ensureGraphOpened(moduleName);
    var nodesCnt = moduleGraph.getAllNodes().size() + 1;
    LOGGER.trace("Creating EnsoModuleAST for module {}, nodeId = {}", moduleName, nodesCnt);
    var moduleAst = EnsoModuleAST.fromIR(ir, srcFile, moduleName, nodesCnt);
    moduleGraph.addSubGraphForPass(afterPass, moduleAst);
  }

  private ModuleGraph ensureGraphOpened(String moduleName) {
    if (!moduleGraphs.containsKey(moduleName)) {
      LOGGER.trace("Opening graph for module {}", moduleName);
      var graph = new ModuleGraph(moduleName);
      moduleGraphs.put(moduleName, graph);
    }
    return moduleGraphs.get(moduleName);
  }

  @Override
  public void close() {
    for (var entry : moduleGraphs.entrySet()) {
      var modName = entry.getKey();
      var moduleGraph = entry.getValue();
      var outPath = outputForModule(modName);
      LOGGER.trace("Dumping graph for module {} to {}", modName, outPath);
      try {
        moduleGraph.dump(outPath);
      } catch (IOException e) {
        LOGGER.error("Failed to dump graph for module {}", modName, e);
      }
    }
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
    var irPath = irDumpsDir.resolve(moduleName + ".bgv");
    if (!irPath.toFile().exists()) {
      try {
        Files.createFile(irPath);
      } catch (IOException e) {
        throw new IllegalStateException(e);
      }
    }
    return irPath;
  }
}
