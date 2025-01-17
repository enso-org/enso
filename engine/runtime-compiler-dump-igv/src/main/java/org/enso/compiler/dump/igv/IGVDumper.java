package org.enso.compiler.dump.igv;

import java.io.File;
import java.io.IOException;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumpService;
import org.graalvm.graphio.GraphOutput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IGVDumper implements IRDumpService {

  private static final String DEFAULT_DUMP_DIR = "ir-dumps";
  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumper.class);

  @Override
  public void dump(Module ir, String moduleName, File srcFile, String afterPass) {
    LOGGER.trace("Dumping IR for module {} after pass {} in IGV", moduleName, afterPass);
    var ensoAst = EnsoModuleAST.fromIR(ir, srcFile);
    var groupName = afterPass;
    var shortName = afterPass;
    var outPath = outputForModule(moduleName);
    try (var dumpChannel = createFileChannel(outPath)) {
      var output =
          GraphOutput.newBuilder(EnsoModuleAST.AST_DUMP_STRUCTURE)
              .blocks(EnsoModuleAST.AST_DUMP_STRUCTURE)
              .elementsAndLocations(
                  EnsoModuleAST.AST_DUMP_STRUCTURE, EnsoModuleAST.AST_DUMP_STRUCTURE)
              .build(dumpChannel);
      var properties = new HashMap<>();
      output.beginGroup(ensoAst, groupName, shortName, null, 0, null);
      output.print(ensoAst, properties, 0, "%s", moduleName);
      output.endGroup();
      output.close();
    } catch (IOException e) {
      throw new IllegalStateException("Failed to dump Enso AST", e);
    }
    LOGGER.debug("IR dumped in {}", outPath);
  }

  private static WritableByteChannel createFileChannel(Path path) {
    try {
      return Files.newByteChannel(path, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    } catch (IOException e) {
      throw new IllegalStateException(e);
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
