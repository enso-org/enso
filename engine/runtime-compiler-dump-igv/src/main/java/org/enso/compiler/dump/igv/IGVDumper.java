package org.enso.compiler.dump.igv;

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
  public void dump(Module ir, String moduleName) {
    LOGGER.info("Dumping IR for module {} in IGV", moduleName);
    var ensoAst = EnsoAST.fromIR(ir);
    var groupName = "Enso: " + moduleName;
    var shortName = "Enso: " + moduleName.substring(moduleName.lastIndexOf('.') + 1);
    var outPath = outputForModule(moduleName);
    try (var dumpChannel = createFileChannel(outPath)) {
      var output = GraphOutput.newBuilder(EnsoAST.AST_DUMP_STRUCTURE).build(dumpChannel);
      var properties = new HashMap<>();
      var format = "%s";
      output.beginGroup(ensoAst, groupName, shortName, null, 0, null);
      output.print(ensoAst, properties, 0, format);
      output.endGroup();
      output.close();
    } catch (IOException e) {
      throw new IllegalStateException("Failed to dump Enso AST", e);
    }
    LOGGER.info("IR dumped in {}", outPath);
  }

  private static WritableByteChannel createFileChannel(Path path) {
    try {
      return Files.newByteChannel(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
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
