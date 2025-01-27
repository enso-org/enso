package org.enso.compiler.dump.igv;

import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumper;
import org.graalvm.graphio.GraphOutput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IGVDumper implements IRDumper {

  private static final String DEFAULT_DUMP_DIR = "ir-dumps";
  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumper.class);
  private static final int DEFAULT_IGV_PORT = 4445;
  private final String moduleName;
  // Created lazily
  private GraphOutput<EnsoModuleAST, ASTMethod> graphOutput;
  private int currGraphId;
  private boolean groupCreated;

  private final Map<UUID, Integer> nodeIds = new HashMap<>();

  private IGVDumper(String moduleName) {
    this.moduleName = moduleName;
  }

  static IGVDumper createForModule(String moduleName) {
    return new IGVDumper(moduleName);
  }

  private GraphOutput<EnsoModuleAST, ASTMethod> graphOutput() {
    if (graphOutput == null) {
      var channel = createChannel(moduleName);
      try {
        graphOutput =
            GraphOutput.newBuilder(EnsoModuleAST.AST_DUMP_STRUCTURE)
                .blocks(EnsoModuleAST.AST_DUMP_STRUCTURE)
                .elementsAndLocations(
                    EnsoModuleAST.AST_DUMP_STRUCTURE, EnsoModuleAST.AST_DUMP_STRUCTURE)
                .attr("type", "Enso IR")
                .build(channel);
      } catch (IOException e) {
        throw new IllegalStateException(
            "Failed to create graph output for module " + moduleName, e);
      }
    }
    return graphOutput;
  }

  private static WritableByteChannel createChannel(String moduleName) {
    WritableByteChannel channel;
    try {
      channel = SocketChannel.open(new InetSocketAddress(DEFAULT_IGV_PORT));
      LOGGER.info("Connected to IGV");
    } catch (IOException e) {
      var outPath = outputForModule(moduleName);
      LOGGER.info("Failed to connect to IGV. Graph will be dumped in {}", outPath);
      channel = createFileChannel(outPath);
    }
    return channel;
  }

  @Override
  public void dumpModule(Module ir, String graphName, File srcFile, String afterPass) {
    assert graphName.equals(this.moduleName);
    dumpTask(ir, graphName, srcFile, afterPass);
  }

  @Override
  public void dumpExpression(Expression expr, String graphName, String afterPass) {
    dumpTask(expr, graphName, null, afterPass);
  }

  private void dumpTask(IR ir, String moduleName, File srcFile, String afterPass) {
    LOGGER.trace("[{}] Creating EnsoModuleAST after pass {}", moduleName, afterPass);
    EnsoModuleAST moduleAst;
    if (ir instanceof Module moduleIr) {
      moduleAst = EnsoModuleAST.fromModuleIR(moduleIr, srcFile, moduleName, nodeIds);
    } else if (ir instanceof Expression expr) {
      moduleAst = EnsoModuleAST.fromExpressionIR(expr, moduleName, nodeIds);
    } else {
      throw new IllegalArgumentException("Unsupported IR type: " + ir.getClass());
    }
    try {
      if (!groupCreated) {
        var groupProps = groupProps(moduleName, moduleAst);
        graphOutput().beginGroup(moduleAst, moduleName, moduleName, null, 0, groupProps);
        groupCreated = true;
      }
      LOGGER.trace("[{}] Printing module AST with ID {}", moduleName, currGraphId);
      var graphProps = graphProps(afterPass);
      graphOutput().print(moduleAst, graphProps, currGraphId, "%s", afterPass);
    } catch (IOException e) {
      LOGGER.error("[{}] Failed to dump the graph for pass {}", moduleName, afterPass);
      throw new RuntimeException(e);
    }
    currGraphId++;
    LOGGER.trace("[{}] Dumped after pass {}", moduleName, afterPass);
  }

  private static Map<String, Object> groupProps(String moduleName, EnsoModuleAST graph) {
    var props = new HashMap<String, Object>();
    props.put("moduleName", moduleName);
    props.put("date", LocalDateTime.now());
    props.put("srcFile", graph.getSrcFile() == null ? null : graph.getSrcFile().getAbsolutePath());
    return props;
  }

  private static Map<String, Object> graphProps(String afterPass) {
    var props = new HashMap<String, Object>();
    props.put("passName", afterPass);
    return props;
  }

  @Override
  public void close() {
    if (groupCreated) {
      assert graphOutput != null;
      try {
        graphOutput.endGroup();
      } catch (IOException e) {
        LOGGER.error("[%s] Failed to end the group".formatted(moduleName), e);
        return;
      }
      graphOutput.close();
      LOGGER.trace("[{}] Graph dumped", moduleName);
    } else {
      LOGGER.trace("[{}] No graphs to dump", moduleName);
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
          path,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE,
          StandardOpenOption.TRUNCATE_EXISTING);
    } catch (IOException e) {
      throw new IllegalStateException(
          "Failed to create byte channel to file " + path.toAbsolutePath(), e);
    }
  }
}
