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
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Definition.Data;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedModuleMethod;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotField;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotSymbol;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.compiler.pass.analyse.alias.AliasMetadata;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.resolve.FullyQualifiedNames.FQNResolution;
import org.enso.compiler.pass.resolve.FullyQualifiedNames.ResolvedLibrary;
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

  public static final String DEFAULT_DUMP_DIR = "ir-dumps";
  public static final String SYSTEM_PROP = "enso.compiler.dumpIr";

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
        var methodRef = explicitMethodIr.methodReference();
        createIRGraph(methodRef);
        createEdge(explicitMethodIr, methodRef, "methodReference");
      }
      case Method.Conversion conversionMethod -> {
        var bldr =
            GraphVizNode.Builder.fromIr(conversionMethod)
                .addLabelLine("methodName: " + conversionMethod.methodName().name());
        addNode(bldr.build());
        var body = conversionMethod.body();
        createIRGraph(body);
        createEdge(conversionMethod, body, "body");
        var methodRef = conversionMethod.methodReference();
        createIRGraph(methodRef);
        createEdge(conversionMethod, methodRef, "methodReference");
      }
      case Method.Binding binding -> {
        var bldr = GraphVizNode.Builder.fromIr(binding);
        addNode(bldr.build());
        for (int i = 0; i < binding.arguments().size(); i++) {
          var arg = binding.arguments().apply(i);
          createIRGraph(arg);
          createEdge(binding, arg, "arg[" + i + "]");
        }
        var body = binding.body();
        createIRGraph(body);
        createEdge(binding, body, "body");
        var methodRef = binding.methodReference();
        createIRGraph(methodRef);
        createEdge(binding, methodRef, "methodReference");
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
      case Name.GenericAnnotation genericAnnotation -> {
        var bldr =
            GraphVizNode.Builder.fromIr(genericAnnotation)
                .addLabelLine("name: " + genericAnnotation.name())
                .addLabelLine("isMethod: " + genericAnnotation.isMethod());
        addNode(bldr.build());
        var expr = genericAnnotation.expression();
        createIRGraph(expr);
        createEdge(genericAnnotation, expr, "expression");
      }
      case Name.BuiltinAnnotation builtinAnnotation -> {
        var bldr =
            GraphVizNode.Builder.fromIr(builtinAnnotation)
                .addLabelLine("name: " + builtinAnnotation.name());
        addNode(bldr.build());
      }
      case org.enso.compiler.core.ir.Type.Ascription ascription -> {
        var ascriptionNode = GraphVizNode.Builder.fromIr(ascription).build();
        addNode(ascriptionNode);
        var typed = ascription.typed();
        createIRGraph(typed);
        createEdge(ascription, typed, "typed");
        var signature = ascription.signature();
        createIRGraph(signature);
        createEdge(ascription, signature, "signature");
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
      case Case.Expr caseExpr -> {
        var isNested = caseExpr.isNested();
        var caseNode =
            GraphVizNode.Builder.fromIr(caseExpr).addLabelLine("isNested: " + isNested).build();
        addNode(caseNode);
        var scrutineeExpr = caseExpr.scrutinee();
        createIRGraph(scrutineeExpr);
        createEdge(caseExpr, scrutineeExpr, "scrutinee");
        var branches = caseExpr.branches();
        for (int i = 0; i < branches.size(); i++) {
          var branch = branches.apply(i);
          createIRGraph(branch);
          createEdge(caseExpr, branch, "branch[" + i + "]");
        }
      }
      case Case.Branch caseBranch -> {
        var isTerminalBranch = caseBranch.terminalBranch();
        var caseBranchNode =
            GraphVizNode.Builder.fromIr(caseBranch)
                .addLabelLine("terminalBranch: " + isTerminalBranch)
                .build();
        addNode(caseBranchNode);
        var pattern = caseBranch.pattern();
        createIRGraph(pattern);
        createEdge(caseBranch, pattern, "pattern");
        var expr = caseBranch.expression();
        createIRGraph(expr);
        createEdge(caseBranch, expr, "expression");
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
        createEdge(lambda, body, "body");
        for (int i = 0; i < lambda.arguments().size(); i++) {
          var arg = lambda.arguments().apply(i);
          createIRGraph(arg);
          createEdge(lambda, arg, "arg[" + i + "]");
        }
      }
      case Expression.Binding exprBinding -> {
        var exprBindNode =
            GraphVizNode.Builder.fromIr(exprBinding)
                .addLabelLine("name: " + exprBinding.name().name())
                .build();
        addNode(exprBindNode);
        createIRGraph(exprBinding.expression());
        createEdge(exprBinding, exprBinding.expression(), "expression");
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
      case Name.MethodReference methodRef -> {
        var bldr = GraphVizNode.Builder.fromIr(methodRef);
        bldr.addLabelLine("methodName: " + methodRef.methodName().name());
        if (methodRef.typePointer().isDefined()) {
          bldr.addLabelLine("typePointer: " + methodRef.typePointer().get().name());
        } else {
          bldr.addLabelLine("typePointer: null");
        }
        var methodRefNode = bldr.build();
        addNode(methodRefNode);
      }
      default -> {
        var node = GraphVizNode.Builder.fromIr(expression).build();
        addNode(node);
      }
    }
  }

  private void createIRGraph(Pattern pattern) {
    var bldr = GraphVizNode.Builder.fromIr(pattern);
    switch (pattern) {
      case Pattern.Constructor constrPat -> {
        var constr = constrPat.constructor();
        bldr.addLabelLine("constructor: " + constr.name());
        addNode(bldr.build());
        var fields = constrPat.fields();
        for (int i = 0; i < fields.size(); i++) {
          var field = fields.apply(i);
          createIRGraph(field);
          createEdge(constrPat, field, "field[" + i + "]");
        }
      }
      case Pattern.Type tp -> {
        addNode(bldr.build());
        var name = tp.name();
        var tpe = tp.tpe();
        createIRGraph(name);
        createIRGraph(tpe);
        createEdge(tp, name, "name");
        createEdge(tp, tpe, "tpe");
      }
      case Pattern.Literal litPat -> {
        addNode(bldr.build());
        var lit = litPat.literal();
        createIRGraph(lit);
        createEdge(litPat, lit, "literal");
      }
      case Pattern.Name name -> {
        bldr.addLabelLine("name: " + name.name().name());
        addNode(bldr.build());
      }
      case Pattern.Documentation doc -> {
        bldr.addLabelLine("doc: " + doc.doc());
        addNode(bldr.build());
      }
      default -> throw unimpl(pattern);
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
                .addLabelLine("name: " + importModIr.name().name())
                .addLabelLine("isAll: " + importModIr.isAll());
        if (importModIr.rename().isDefined()) {
          var rename = importModIr.rename().get();
          bldr.addLabelLine("rename: " + rename.name());
        } else {
          bldr.addLabelLine("rename: null");
        }
        addNode(bldr.build());
      }
      case Polyglot polyImport -> {
        var bldr = GraphVizNode.Builder.fromIr(polyImport);
        bldr.addLabelLine(
            "entity: Entity(langName="
                + polyImport.entity().langName()
                + ", visibleName="
                + polyImport.entity().getVisibleName()
                + ")");
        if (polyImport.rename().isDefined()) {
          var rename = polyImport.rename().get();
          bldr.addLabelLine("rename: " + rename);
        } else {
          bldr.addLabelLine("rename: null");
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
                case ResolvedModuleMethod resolvedModuleMethod -> {
                  bldr.addLabelLine(
                      "target: ResolvedMethod(" + resolvedModuleMethod.method().name() + ")");
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
                case ResolvedLibrary resolvedLibrary -> {
                  bldr.addLabelLine("target: ResolvedLibrary(" + resolvedLibrary.namespace() + ")");
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
            case BindingsMap bindingsMap -> {
              if (bindingsMap.definedEntities().isEmpty()) {
                bldr.addLabelLine("definedEntities: []");
              } else {
                bldr.addLabelLine("definedEntities: ");
                for (int i = 0; i < bindingsMap.definedEntities().size(); i++) {
                  var entity = bindingsMap.definedEntities().apply(i);
                  switch (entity) {
                    case BindingsMap.Type tp -> bldr.addLabelLine("  - Type(" + tp.name() + ")");
                    case BindingsMap.ModuleMethod method -> bldr.addLabelLine(
                        "  - ModuleMethod(" + method.name() + ")");
                    case BindingsMap.PolyglotSymbol polySym -> bldr.addLabelLine(
                        "  - PolyglotSymbol(" + polySym.name() + ")");
                    case BindingsMap.ExtensionMethod extensionMethod -> bldr.addLabelLine(
                        "  - ExtensionMethod(" + extensionMethod.name() + ")");
                    case BindingsMap.ConversionMethod conversionMethod -> bldr.addLabelLine(
                        "  - ConversionMethod(" + conversionMethod.name() + ")");
                    default -> throw unimpl(entity);
                  }
                }
              }

              if (bindingsMap.resolvedImports().isEmpty()) {
                bldr.addLabelLine("resolvedImports: []");
              } else {
                bldr.addLabelLine("resolvedImports: ");
                for (int i = 0; i < bindingsMap.resolvedImports().size(); i++) {
                  var resolvedImport = bindingsMap.resolvedImports().apply(i);
                  var firstImpTarget = resolvedImport.targets().head();
                  switch (firstImpTarget) {
                    case ResolvedType resolvedType -> bldr.addLabelLine(
                        "  - ResolvedType(" + resolvedType.tp().name() + ")");
                    case BindingsMap.ResolvedModule resolvedModule -> bldr.addLabelLine(
                        "  - ResolvedModule(" + resolvedModule.qualifiedName() + ")");
                    default -> throw unimpl(firstImpTarget);
                  }
                }
              }
              var bmNode = bldr.build();
              addNode(bmNode);
              createEdge(ir, bindingsMap, "BindingsMap");
            }
            case AliasMetadata.Occurrence occurence -> {
              bldr.addLabelLine("occurenceId: " + occurence.id());
              addNode(bldr.build());
              createEdge(ir, occurence, "Alias.Info.Occurence");
            }
            case AliasMetadata.RootScope rootScope -> {
              addAliasGraphScopeLabels(bldr, rootScope.graph().rootScope());
              var aliasNode = bldr.build();
              addNode(aliasNode);
              createEdge(ir, rootScope, "Alias.Info.Scope.Root");
            }
            case AliasMetadata.ChildScope childScope -> {
              addAliasGraphScopeLabels(bldr, childScope.scope());
              var aliasNode = bldr.build();
              addNode(aliasNode);
              createEdge(ir, childScope, "Alias.Info.Scope.Child");
            }
              // The rest is ignored
            default -> {}
          }
          return null;
        });
  }

  private void addAliasGraphScopeLabels(GraphVizNode.Builder bldr, Graph.Scope scope) {
    var parent = scope.parent();
    if (parent.isDefined()) {
      var parentId = Utils.id(parent.get());
      bldr.addLabelLine("parent: " + parentId);
    } else {
      bldr.addLabelLine("parent: null");
    }
    var occurences = scope.occurrences();
    if (occurences.isEmpty()) {
      bldr.addLabelLine("occurrences: []");
    } else {
      bldr.addLabelLine("occurrences: ");
      occurences
          .values()
          .foreach(
              occ -> {
                bldr.addLabelLine("  - " + occ);
                return null;
              });
    }
    var childScopes = scope.childScopes();
    if (childScopes.isEmpty()) {
      bldr.addLabelLine("childScopes: []");
    } else {
      bldr.addLabelLine("childScopes: ");
      childScopes.foreach(
          childScope -> {
            var id = Utils.id(childScope);
            bldr.addLabelLine("  - " + id);
            return null;
          });
    }
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

  /** Just a wrapper for code, we need this to be able to add the code to the graph. */
  private record Code(String code) {

    private Code(String code) {
      // Replace new lines with left-justify literals, so that all the lines
      // in the code are justified to the left side of the box.
      String formattedCode = code.replace("\r", "\\l").replace("\n", "\\l");
      if (code.contains("\"")) {
        formattedCode = formattedCode.replace("\"", "\\\"");
      }
      assert Utils.hasOneLine(formattedCode);
      this.code = formattedCode;
    }
  }
}
