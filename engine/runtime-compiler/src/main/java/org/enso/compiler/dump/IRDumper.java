package org.enso.compiler.dump;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Literal.Number;
import org.enso.compiler.core.ir.Literal.Text;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Definition.Data;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedMethod;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotField;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotSymbol;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.compiler.pass.resolve.FullyQualifiedNames.FQNResolution;
import org.enso.compiler.pass.resolve.FullyQualifiedNames.ResolvedLibraryNamespace;
import org.enso.compiler.pass.resolve.FullyQualifiedNames.ResolvedModule;

/**
 * Utility class that dumps {@link IR} to a <a href="https://graphviz.org">GraphViz</a> file. This
 * file can be processed by a {@code dot} command to generate a visual representation of the IR. The
 * GraphViz command line utilities are easy to install. See the <a
 * href="https://graphviz.org/download/">download page</a>. Alternatively, the resulting file can be
 * interactivelly visualized in VSCode with the <a
 * href="https://marketplace.visualstudio.com/items?itemName=tintinweb.graphviz-interactive-preview">GraphViz
 * Interactive Preview extension</a>.
 */
public class IRDumper {
  /**
   * Whether to include the code of the IR nodes in the Graphviz file. This can make the file very
   * large.
   */
  private static final boolean INCLUDE_CODE = true;

  /** Whether to include some pass data in the GraphViz file. */
  private static final boolean INCLUDE_PASS_DATA = true;

  private final OutputStream out;
  private final Set<GraphVizNode> nodes = new HashSet<>();
  private final Set<GraphVizEdge> edges = new HashSet<>();

  /**
   * @param out the output stream to write the Graphviz file to.
   */
  private IRDumper(OutputStream out) {
    Objects.requireNonNull(out);
    this.out = out;
  }

  /**
   * Creates a new {@link IRDumper} that dumps the graph into the given {@code path}.
   *
   * @param path the path to write the Graphviz file to.
   */
  public static IRDumper fromPath(Path path) {
    OutputStream out = null;
    try {
      out =
          Files.newOutputStream(
              path,
              StandardOpenOption.CREATE,
              StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING);
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
    return new IRDumper(out);
  }

  /**
   * Creates a new {@link IRDumper} that dumps the graph into the given {@code out}.
   *
   * @param out the output stream to write the Graphviz file to.
   */
  public static IRDumper fromOut(OutputStream out) {
    return new IRDumper(out);
  }

  /**
   * Dumps the given IR into the Graphviz file. Any {@link IOException} is translated to a {@link
   * IllegalStateException} within this class.
   *
   * @param ir the IR to dump.
   */
  public void dump(IR ir) {
    createIRGraph(ir);
    dumpGraph();
    try {
      out.flush();
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  private void createIRGraph(IR ir) {
    switch (ir) {
      case Module moduleIr -> createIRGraph(moduleIr);
      default -> throw unimpl(ir);
    }
  }

  private void createIRGraph(Module moduleIr) {
    var moduleNode = GraphVizNode.Builder.fromIr(moduleIr).build();
    addNode(moduleNode);

    for (int i = 0; i < moduleIr.bindings().size(); i++) {
      var bindingIr = moduleIr.bindings().apply(i);
      createIRGraph(bindingIr);
      var edgeDescr = "binding[" + i + "]";
      createEdge(moduleIr, bindingIr, edgeDescr);
    }

    for (int i = 0; i < moduleIr.imports().size(); i++) {
      var importIr = moduleIr.imports().apply(i);
      createIRGraph(importIr);
      var edgeDescr = "import[" + i + "]";
      createEdge(moduleIr, importIr, edgeDescr);
    }

    for (int i = 0; i < moduleIr.exports().size(); i++) {
      var exportIr = moduleIr.exports().apply(i);
      createIRGraph(exportIr);
      var edgeDescr = "export[" + i + "]";
      createEdge(moduleIr, exportIr, edgeDescr);
    }
  }

  private void createIRGraph(Definition definitionIr) {
    switch (definitionIr) {
      case Method.Explicit explicitMethodIr -> {
        var bldr =
            GraphVizNode.Builder.fromIr(explicitMethodIr)
                .addLabelLine("methodName: " + explicitMethodIr.methodName().name())
                .addLabelLine("isStatic: " + explicitMethodIr.isStatic());
        if (explicitMethodIr.typeName().isDefined()) {
          bldr.addLabelLine("typeName: " + explicitMethodIr.typeName().get().name());
        } else {
          bldr.addLabelLine("typeName: null");
        }
        addNode(bldr.build());
        var body = explicitMethodIr.body();
        createIRGraph(body);
        createEdge(explicitMethodIr, body, "body");
      }
      case Definition.Type type -> {
        var typeNode =
            GraphVizNode.Builder.fromIr(type).addLabelLine("name: " + type.name().name()).build();
        addNode(typeNode);
        for (int i = 0; i < type.members().size(); i++) {
          var member = type.members().apply(i);
          createIRGraph(member);
          createEdge(type, member, "member[" + i + "]");
        }
      }
      default -> throw unimpl(definitionIr);
    }
  }

  private void createIRGraph(Data atomCons) {
    var consNode =
        GraphVizNode.Builder.fromIr(atomCons)
            .addLabelLine("name: " + atomCons.name().name())
            .build();
    addNode(consNode);
    for (int i = 0; i < atomCons.arguments().size(); i++) {
      var arg = atomCons.arguments().apply(i);
      createIRGraph(arg);
      createEdge(atomCons, arg, "arg[" + i + "]");
    }
  }

  private void createIRGraph(Expression expression) {
    switch (expression) {
      case Expression.Block block -> {
        var blockNode = GraphVizNode.Builder.fromIr(block).build();
        addNode(blockNode);
        for (int i = 0; i < block.expressions().size(); i++) {
          var expr = block.expressions().apply(i);
          createIRGraph(expr);
          createEdge(block, expr, "expression[" + i + "]");
        }
        var retVal = block.returnValue();
        createIRGraph(retVal);
        createEdge(block, retVal, "returnValue");
      }
      case Application.Prefix prefixApp -> {
        var prefixAppNode =
            GraphVizNode.Builder.fromIr(prefixApp)
                .addLabelLine("hasDefaultsSuspended: " + prefixApp.hasDefaultsSuspended())
                .build();
        addNode(prefixAppNode);

        var func = prefixApp.function();
        createIRGraph(func);
        createEdge(prefixApp, func, "function");

        for (int i = 0; i < prefixApp.arguments().size(); i++) {
          var arg = prefixApp.arguments().apply(i);
          createIRGraph(arg);
          createEdge(prefixApp, arg, "arg[" + i + "]");
        }
      }
      case Function.Lambda lambda -> {
        var lambdaNode = GraphVizNode.Builder.fromIr(lambda).build();
        addNode(lambdaNode);
        var body = lambda.body();
        createIRGraph(body);
        createEdge((lambda), (body), "body");
        for (int i = 0; i < lambda.arguments().size(); i++) {
          var arg = lambda.arguments().apply(i);
          createIRGraph(arg);
          createEdge(lambda, arg, "arg[" + i + "]");
        }
      }
      case Number number -> {
        var numNode =
            GraphVizNode.Builder.fromIr(number).addLabelLine("value: " + number.value()).build();
        addNode(numNode);
      }
      case Text text -> {
        var textNode =
            GraphVizNode.Builder.fromIr(text).addLabelLine("text: " + text.text()).build();
        addNode(textNode);
      }
      case Name.Literal literal -> {
        var bldr = GraphVizNode.Builder.fromIr(literal);
        bldr.addLabelLine("name: " + literal.name());
        bldr.addLabelLine("isMethod: " + literal.isMethod());
        if (literal.originalName().isDefined()) {
          var origName = literal.originalName().get();
          bldr.addLabelLine("originalName: " + origName.name());
        } else {
          bldr.addLabelLine("originalName: null");
        }
        var literalNode = bldr.build();
        addNode(literalNode);
      }
      default -> {
        var node = GraphVizNode.Builder.fromIr(expression).build();
        addNode(node);
      }
    }
  }

  private void createIRGraph(CallArgument argument) {
    switch (argument) {
      case CallArgument.Specified specifiedArg -> {
        var bldr = GraphVizNode.Builder.fromIr(specifiedArg);
        if (specifiedArg.name().isDefined()) {
          bldr.addLabelLine("name: " + specifiedArg.name().get().name());
        } else {
          bldr.addLabelLine("name: null");
        }
        addNode(bldr.build());

        var value = specifiedArg.value();
        createIRGraph(value);
        createEdge(specifiedArg, value, "value");
      }
      default -> throw unimpl(argument);
    }
  }

  private void createIRGraph(DefinitionArgument argument) {
    switch (argument) {
      case DefinitionArgument.Specified specifiedArg -> {
        var bldr =
            GraphVizNode.Builder.fromIr(specifiedArg)
                .addLabelLine("name: " + specifiedArg.name().name())
                .addLabelLine("suspended: " + specifiedArg.suspended());
        var node = bldr.build();
        addNode(node);

        if (specifiedArg.ascribedType().isDefined()) {
          var ascribedType = specifiedArg.ascribedType().get();
          createIRGraph(ascribedType);
          createEdge(specifiedArg, ascribedType, "ascribedType");
        }
        if (specifiedArg.defaultValue().isDefined()) {
          var defaultValue = specifiedArg.defaultValue().get();
          createIRGraph(defaultValue);
          createEdge(specifiedArg, defaultValue, "defaultValue");
        }
      }
      default -> throw unimpl(argument);
    }
  }

  private void createIRGraph(Import importIr) {
    switch (importIr) {
      case Import.Module importModIr -> {
        var bldr =
            GraphVizNode.Builder.fromIr(importModIr)
                .addLabelLine("isSynthetic: " + importModIr.isSynthetic())
                .addLabelLine("name: " + importModIr.name().name().toString())
                .addLabelLine("isAll: " + importModIr.isAll());
        if (importModIr.rename().isDefined()) {
          var rename = importModIr.rename().get();
          bldr.addLabelLine("rename: " + rename.name());
        }
        addNode(bldr.build());
      }
      default -> throw unimpl(importIr);
    }
  }

  private void createIRGraph(Export exportIr) {
    switch (exportIr) {
      case Export.Module exportModIr -> {
        var node =
            GraphVizNode.Builder.fromIr(exportIr)
                .addLabelLine("isSynthetic: " + exportModIr.isSynthetic())
                .addLabelLine("name: " + exportModIr.name().name())
                .addLabelLine("isAll: " + exportModIr.isAll())
                .build();
        addNode(node);
      }
      default -> throw unimpl(exportIr);
    }
  }

  private void createPassDataGraph(IR ir) {
    var passData = ir.passData();
    passData.map(
        (pass, data) -> {
          var bldr = GraphVizNode.Builder.fromObject(data);
          bldr.addAttribute("shape", "box");
          bldr.addAttribute("color", "blue");
          bldr.addLabelLine("metadataName: " + data.metadataName());
          switch (data) {
            case BindingsMap.Resolution resolution -> {
              switch (resolution.target()) {
                case BindingsMap.ResolvedModule resolvedModule -> {
                  bldr.addLabelLine(
                      "target: ResolvedModule("
                          + resolvedModule.module().getName().toString()
                          + ")");
                }
                case ResolvedConstructor resolvedConstructor -> {
                  bldr.addLabelLine(
                      "target: ResolvedConstructor(" + resolvedConstructor.cons().name() + ")");
                }
                case ResolvedMethod resolvedMethod -> {
                  bldr.addLabelLine(
                      "target: ResolvedMethod(" + resolvedMethod.method().name() + ")");
                }
                case ResolvedPolyglotField resolvedPolyglotField -> {
                  bldr.addLabelLine(
                      "target: ResolvedPolyglotField(" + resolvedPolyglotField.name() + ")");
                }
                case ResolvedPolyglotSymbol resolvedPolyglotSymbol -> {
                  bldr.addLabelLine(
                      "target: ResolvedPolyglotSymbol("
                          + resolvedPolyglotSymbol.symbol().name()
                          + ")");
                }
                case ResolvedType resolvedType -> {
                  bldr.addLabelLine("target: ResolvedType(" + resolvedType.tp().name() + ")");
                }
                default -> throw unimpl(resolution.target());
              }
              var metaNode = bldr.build();
              addNode(metaNode);
              createEdge(ir, resolution, "BindingsMap.Resolution");
            }
            case FQNResolution fqnResolution -> {
              switch (fqnResolution.target()) {
                case ResolvedLibraryNamespace resolvedLibraryNamespace -> {
                  bldr.addLabelLine(
                      "target: ResolvedLibraryNamespace("
                          + resolvedLibraryNamespace.namespace()
                          + ")");
                }
                case ResolvedModule resolvedModule -> {
                  bldr.addLabelLine(
                      "target: ResolvedModule("
                          + resolvedModule.moduleRef().getName().toString()
                          + ")");
                }
                default -> throw unimpl(fqnResolution.target());
              }
              var fqnMetaNode = bldr.build();
              addNode(fqnMetaNode);
              createEdge(ir, fqnResolution, "FullyQualifiedNames.FQNResolution");
            }
              // The rest is ignored
            default -> {}
          }
          return null;
        });
  }

  private void addNode(GraphVizNode node) {
    var isNodeAlreadyDefined = nodes.stream().anyMatch(n -> n.equals(node));
    if (isNodeAlreadyDefined) {
      // Skip duplicate nodes.
      return;
    }
    nodes.add(node);
    if (INCLUDE_CODE) {
      if (node.object() instanceof IR ir) {
        var code = new Code(ir.showCode());
        var codeNode =
            GraphVizNode.Builder.fromObjectPlain(code)
                .addAttribute("shape", "box")
                .addAttribute("color", "grey")
                .addLabelLine(code.code)
                .build();
        nodes.add(codeNode);
        var edgeAttrs = Map.of("color", "grey", "style", "dotted");
        createEdge(ir, code, "code", edgeAttrs);
      }
    }
    if (INCLUDE_PASS_DATA) {
      if (node.object() instanceof IR ir) {
        createPassDataGraph(ir);
      }
    }
  }

  private void createEdge(Object from, Object to, String label, Map<String, String> attrs) {
    assert !(from instanceof String);
    assert !(to instanceof String);
    assert !(from instanceof GraphVizNode);
    assert !(to instanceof GraphVizNode);
    var fromId = Utils.id(from);
    var toId = Utils.id(to);
    var edge = GraphVizEdge.newEdgeWithAttributes(fromId, toId, label, attrs);
    var nodesContainsFrom = nodes.stream().anyMatch(node -> node.id().equals(fromId));
    var nodesContainsTo = nodes.stream().anyMatch(node -> node.id().equals(toId));
    assert nodesContainsFrom
        : "Node " + fromId + " not found. You must first create it before creating an edge from it";
    assert nodesContainsTo
        : "Node " + toId + " not found. You must first create it before creating an edge to it";
    var edgeAlreadyExists = edges.stream().anyMatch(e -> e.equals(edge));
    if (!edgeAlreadyExists) {
      edges.add(edge);
    }
  }

  private void createEdge(Object from, Object to, String label) {
    createEdge(from, to, label, Map.of());
  }

  /** Dump all the nodes and edges definitions into the GraphViz format. */
  private void dumpGraph() {
    write("digraph {");
    write(System.lineSeparator());
    for (GraphVizNode node : nodes) {
      var nodeRepr = node.toGraphViz();
      write(nodeRepr);
      write(System.lineSeparator());
    }
    for (GraphVizEdge edge : edges) {
      var containsFromNode = nodes.stream().anyMatch(node -> node.id().equals(edge.from()));
      var containsToNode = nodes.stream().anyMatch(node -> node.id().equals(edge.to()));
      assert containsFromNode;
      assert containsToNode;
      var edgeRepr = edge.toGraphViz();
      write(edgeRepr);
      write(System.lineSeparator());
    }
    write("}");
  }

  private static RuntimeException unimpl(Object obj) {
    throw new UnsupportedOperationException(obj.getClass().getName());
  }

  private void write(String str) {
    try {
      out.write(str.getBytes());
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Just a wrapper for code, we need this to be able to add the code to the graph.
   */
    private record Code(String code) {

    private Code(String code) {
        // Replace new lines with left-justify literals, so that all the lines
        // in the code are justified to the left side of the box.
        String formattedCode = code.replace("\n", "\\l");
        if (code.contains("\"")) {
          formattedCode = formattedCode.replace("\"", "\\\"");
        }
        assert Utils.hasOneLine(formattedCode);
        this.code = formattedCode;
      }
    }
}
