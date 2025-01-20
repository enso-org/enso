package org.enso.compiler.dump.igv;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IGVDumper implements IRDumper {

  private static final String DEFAULT_DUMP_DIR = "ir-dumps";
  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumper.class);
  private final ModuleGraph moduleGraph;

  IGVDumper(String moduleName) {
    this.moduleGraph = new ModuleGraph(moduleName);
  }

  @Override
  public void dump(Module ir, String moduleName, File srcFile, String afterPass) {
    assert moduleName.equals(moduleName());
    var nodesCnt = moduleGraph.getAllNodes().size() + 1;
    LOGGER.trace(
        "[{}] Creating EnsoModuleAST for module {}, nodeId = {}", moduleName, moduleName, nodesCnt);
    var moduleAst = EnsoModuleAST.fromIR(ir, srcFile, moduleName, nodesCnt);
    moduleGraph.addSubGraphForPass(afterPass, moduleAst);
  }

  @Override
  public void close() {
    var modName = moduleGraph.getModuleName();
    var outPath = outputForModule(modName);
    LOGGER.trace("[{}] Dumping graph to {}", modName, outPath);
    try {
      moduleGraph.dump(outPath);
    } catch (IOException e) {
      LOGGER.error("[{}] Failed to dump graph: {}", modName, e);
    }
    LOGGER.trace("[{}] Graph dumped", modName);
  }

  private String moduleName() {
    return moduleGraph.getModuleName();
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
    return irDumpsDir.resolve(moduleName + ".bgv");
  }
}
