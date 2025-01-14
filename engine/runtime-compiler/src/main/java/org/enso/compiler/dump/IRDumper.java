package org.enso.compiler.dump;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
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
 * Dumps the {@link IR IR tree} into a single HTML file that uses <a href="visjs.org">VisJS</a>
 * JavaScript library to (interactivelly) display the graph.
 *
 * <p>Uses <a href="https://visjs.github.io/vis-network/docs/network/">VisJS Network</a> graph type.
 */
public final class IRDumper {
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
  private final Set<JSVizNode> nodes = new HashSet<>();
  private final Set<JSVizEdge> edges = new HashSet<>();

  private IRDumper(OutputStream out) {
    Objects.requireNonNull(out);
    this.out = out;
  }

  /**
   * Creates a new {@link IRDumper} that dumps the graph into the given {@code path}. The {@link
   * #dump(IR, String)} will create a valid HTML file inside the {@code path}.
   *
   * @param path The path to write the HTML file to.
   */
  public static IRDumper fromPath(Path path) {
    OutputStream out;
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
   * Dumps the given IR into the HTML file. Any {@link IOException} is translated to a {@link
   * IllegalStateException} within this class.
   *
   * @param ir the IR to dump.
   */
  public void dump(IR ir, String moduleName) {
    createIRGraph(ir);
    dumpGraph(moduleName);
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
    var moduleNode = JSVizNode.Builder.fromIr(moduleIr).level(0).build();
    addNode(moduleNode);

    for (int i = 0; i < moduleIr.bindings().size(); i++) {
      var bindingIr = moduleIr.bindings().apply(i);
      createIRGraph(bindingIr, 1);
      var edgeDescr = "binding[" + i + "]";
      createEdge(moduleIr, bindingIr, edgeDescr);
    }

    for (int i = 0; i < moduleIr.imports().size(); i++) {
      var importIr = moduleIr.imports().apply(i);
      createIRGraph(importIr, 1);
      var edgeDescr = "import[" + i + "]";
      createEdge(moduleIr, importIr, edgeDescr);
    }

    for (int i = 0; i < moduleIr.exports().size(); i++) {
      var exportIr = moduleIr.exports().apply(i);
      createIRGraph(exportIr, 1);
      var edgeDescr = "export[" + i + "]";
      createEdge(moduleIr, exportIr, edgeDescr);
    }
  }

  private void createIRGraph(Definition definitionIr, int level) {
    switch (definitionIr) {
      case Method.Explicit explicitMethodIr -> {
        var bldr =
            JSVizNode.Builder.fromIr(explicitMethodIr)
                .level(level)
                .addLabelLine("methodName: " + explicitMethodIr.methodName().name())
                .addLabelLine("isStatic: " + explicitMethodIr.isStatic());
        if (explicitMethodIr.typeName().isDefined()) {
          bldr.addLabelLine("typeName: " + explicitMethodIr.typeName().get().name());
        } else {
          bldr.addLabelLine("typeName: null");
        }
        addNode(bldr.build());
        var body = explicitMethodIr.body();
        createIRGraph(body, level + 1);
        createEdge(explicitMethodIr, body, "body");
        var methodRef = explicitMethodIr.methodReference();
        createIRGraph(methodRef, level + 1);
        createEdge(explicitMethodIr, methodRef, "methodReference");
      }
      case Method.Conversion conversionMethod -> {
        var bldr =
            JSVizNode.Builder.fromIr(conversionMethod)
                .level(level)
                .addLabelLine("methodName: " + conversionMethod.methodName().name());
        addNode(bldr.build());
        var body = conversionMethod.body();
        createIRGraph(body, level + 1);
        createEdge(conversionMethod, body, "body");
        var methodRef = conversionMethod.methodReference();
        createIRGraph(methodRef, level + 1);
        createEdge(conversionMethod, methodRef, "methodReference");
      }
      case Method.Binding binding -> {
        var bldr = JSVizNode.Builder.fromIr(binding).level(level);
        addNode(bldr.build());
        for (int i = 0; i < binding.arguments().size(); i++) {
          var arg = binding.arguments().apply(i);
          createIRGraph(arg, level + 1);
          createEdge(binding, arg, "arg[" + i + "]");
        }
        var body = binding.body();
        createIRGraph(body, level + 1);
        createEdge(binding, body, "body");
        var methodRef = binding.methodReference();
        createIRGraph(methodRef, level + 1);
        createEdge(binding, methodRef, "methodReference");
      }
      case Definition.Type type -> {
        var typeNode =
            JSVizNode.Builder.fromIr(type)
                .level(level)
                .addLabelLine("name: " + type.name().name())
                .build();
        addNode(typeNode);
        for (int i = 0; i < type.members().size(); i++) {
          var member = type.members().apply(i);
          createIRGraph(member, level + 1);
          createEdge(type, member, "member[" + i + "]");
        }
      }
      case Name.GenericAnnotation genericAnnotation -> {
        var bldr =
            JSVizNode.Builder.fromIr(genericAnnotation)
                .level(level)
                .addLabelLine("name: " + genericAnnotation.name())
                .addLabelLine("isMethod: " + genericAnnotation.isMethod());
        addNode(bldr.build());
        var expr = genericAnnotation.expression();
        createIRGraph(expr, level + 1);
        createEdge(genericAnnotation, expr, "expression");
      }
      case Name.BuiltinAnnotation builtinAnnotation -> {
        var bldr =
            JSVizNode.Builder.fromIr(builtinAnnotation)
                .level(level)
                .addLabelLine("name: " + builtinAnnotation.name());
        addNode(bldr.build());
      }
      case org.enso.compiler.core.ir.Type.Ascription ascription -> {
        var ascriptionNode = JSVizNode.Builder.fromIr(ascription).level(level).build();
        addNode(ascriptionNode);
        var typed = ascription.typed();
        createIRGraph(typed, level + 1);
        createEdge(ascription, typed, "typed");
        var signature = ascription.signature();
        createIRGraph(signature, level + 1);
        createEdge(ascription, signature, "signature");
      }
      default -> throw unimpl(definitionIr);
    }
  }

  private void createIRGraph(Data atomCons, int level) {
    var consNode =
        JSVizNode.Builder.fromIr(atomCons)
            .level(level)
            .addLabelLine("name: " + atomCons.name().name())
            .build();
    addNode(consNode);
    for (int i = 0; i < atomCons.arguments().size(); i++) {
      var arg = atomCons.arguments().apply(i);
      createIRGraph(arg, level + 1);
      createEdge(atomCons, arg, "arg[" + i + "]");
    }
  }

  private void createIRGraph(Expression expression, int level) {
    switch (expression) {
      case Expression.Block block -> {
        var blockNode = JSVizNode.Builder.fromIr(block).level(level).build();
        addNode(blockNode);
        for (int i = 0; i < block.expressions().size(); i++) {
          var expr = block.expressions().apply(i);
          createIRGraph(expr, level + 1);
          createEdge(block, expr, "expression[" + i + "]");
        }
        var retVal = block.returnValue();
        createIRGraph(retVal, level + 1);
        createEdge(block, retVal, "returnValue");
      }
      case Case.Expr caseExpr -> {
        var isNested = caseExpr.isNested();
        var caseNode =
            JSVizNode.Builder.fromIr(caseExpr)
                .level(level)
                .addLabelLine("isNested: " + isNested)
                .build();
        addNode(caseNode);
        var scrutineeExpr = caseExpr.scrutinee();
        createIRGraph(scrutineeExpr, level + 1);
        createEdge(caseExpr, scrutineeExpr, "scrutinee");
        var branches = caseExpr.branches();
        for (int i = 0; i < branches.size(); i++) {
          var branch = branches.apply(i);
          createIRGraph(branch, level + 1);
          createEdge(caseExpr, branch, "branch[" + i + "]");
        }
      }
      case Case.Branch caseBranch -> {
        var isTerminalBranch = caseBranch.terminalBranch();
        var caseBranchNode =
            JSVizNode.Builder.fromIr(caseBranch)
                .level(level)
                .addLabelLine("terminalBranch: " + isTerminalBranch)
                .build();
        addNode(caseBranchNode);
        var pattern = caseBranch.pattern();
        createIRGraph(pattern, level + 1);
        createEdge(caseBranch, pattern, "pattern");
        var expr = caseBranch.expression();
        createIRGraph(expr, level + 1);
        createEdge(caseBranch, expr, "expression");
      }
      case Application.Prefix prefixApp -> {
        var prefixAppNode =
            JSVizNode.Builder.fromIr(prefixApp)
                .level(level)
                .addLabelLine("hasDefaultsSuspended: " + prefixApp.hasDefaultsSuspended())
                .build();
        addNode(prefixAppNode);

        var func = prefixApp.function();
        createIRGraph(func, level + 1);
        createEdge(prefixApp, func, "function");

        for (int i = 0; i < prefixApp.arguments().size(); i++) {
          var arg = prefixApp.arguments().apply(i);
          createIRGraph(arg, level + 1);
          createEdge(prefixApp, arg, "arg[" + i + "]");
        }
      }
      case org.enso.compiler.core.ir.Function.Lambda lambda -> {
        var lambdaNode = JSVizNode.Builder.fromIr(lambda).level(level).build();
        addNode(lambdaNode);
        var body = lambda.body();
        createIRGraph(body, level + 1);
        createEdge(lambda, body, "body");
        for (int i = 0; i < lambda.arguments().size(); i++) {
          var arg = lambda.arguments().apply(i);
          createIRGraph(arg, level + 1);
          createEdge(lambda, arg, "arg[" + i + "]");
        }
      }
      case Expression.Binding exprBinding -> {
        var exprBindNode =
            JSVizNode.Builder.fromIr(exprBinding)
                .level(level)
                .addLabelLine("name: " + exprBinding.name().name())
                .build();
        addNode(exprBindNode);
        createIRGraph(exprBinding.expression(), level + 1);
        createEdge(exprBinding, exprBinding.expression(), "expression");
      }
      case Number number -> {
        var numNode =
            JSVizNode.Builder.fromIr(number)
                .level(level)
                .addLabelLine("value: " + number.value())
                .build();
        addNode(numNode);
      }
      case Text text -> {
        var textNode =
            JSVizNode.Builder.fromIr(text)
                .level(level)
                .addLabelLine("text: " + text.text())
                .build();
        addNode(textNode);
      }
      case Name.Literal literal -> {
        var bldr = JSVizNode.Builder.fromIr(literal).level(level);
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
        var bldr = JSVizNode.Builder.fromIr(methodRef).level(level);
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
        var node = JSVizNode.Builder.fromIr(expression).level(level).build();
        addNode(node);
      }
    }
  }

  private void createIRGraph(Pattern pattern, int level) {
    var bldr = JSVizNode.Builder.fromIr(pattern).level(level);
    switch (pattern) {
      case Pattern.Constructor constrPat -> {
        var constr = constrPat.constructor();
        bldr.addLabelLine("constructor: " + constr.name());
        addNode(bldr.build());
        var fields = constrPat.fields();
        for (int i = 0; i < fields.size(); i++) {
          var field = fields.apply(i);
          createIRGraph(field, level + 1);
          createEdge(constrPat, field, "field[" + i + "]");
        }
      }
      case Pattern.Type tp -> {
        addNode(bldr.build());
        var name = tp.name();
        var tpe = tp.tpe();
        createIRGraph(name, level + 1);
        createIRGraph(tpe, level + 1);
        createEdge(tp, name, "name");
        createEdge(tp, tpe, "tpe");
      }
      case Pattern.Literal litPat -> {
        addNode(bldr.build());
        var lit = litPat.literal();
        createIRGraph(lit, level + 1);
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

  private void createIRGraph(CallArgument argument, int level) {
    switch (argument) {
      case CallArgument.Specified specifiedArg -> {
        var bldr = JSVizNode.Builder.fromIr(specifiedArg).level(level);
        if (specifiedArg.name().isDefined()) {
          bldr.addLabelLine("name: " + specifiedArg.name().get().name());
        } else {
          bldr.addLabelLine("name: null");
        }
        addNode(bldr.build());

        var value = specifiedArg.value();
        createIRGraph(value, level + 1);
        createEdge(specifiedArg, value, "value");
      }
      default -> throw unimpl(argument);
    }
  }

  private void createIRGraph(DefinitionArgument argument, int level) {
    switch (argument) {
      case DefinitionArgument.Specified specifiedArg -> {
        var bldr =
            JSVizNode.Builder.fromIr(specifiedArg)
                .level(level)
                .addLabelLine("name: " + specifiedArg.name().name())
                .addLabelLine("suspended: " + specifiedArg.suspended());
        var node = bldr.build();
        addNode(node);

        if (specifiedArg.ascribedType().isDefined()) {
          var ascribedType = specifiedArg.ascribedType().get();
          createIRGraph(ascribedType, level + 1);
          createEdge(specifiedArg, ascribedType, "ascribedType");
        }
        if (specifiedArg.defaultValue().isDefined()) {
          var defaultValue = specifiedArg.defaultValue().get();
          createIRGraph(defaultValue, level + 1);
          createEdge(specifiedArg, defaultValue, "defaultValue");
        }
      }
      default -> throw unimpl(argument);
    }
  }

  private void createIRGraph(Import importIr, int level) {
    switch (importIr) {
      case Import.Module importModIr -> {
        var bldr =
            JSVizNode.Builder.fromIr(importModIr)
                .level(level)
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
        var bldr = JSVizNode.Builder.fromIr(polyImport).level(level);
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

  private void createIRGraph(Export exportIr, int level) {
    switch (exportIr) {
      case Export.Module exportModIr -> {
        var node =
            JSVizNode.Builder.fromIr(exportIr)
                .level(level)
                .addLabelLine("isSynthetic: " + exportModIr.isSynthetic())
                .addLabelLine("name: " + exportModIr.name().name())
                .build();
        addNode(node);
      }
      default -> throw unimpl(exportIr);
    }
  }

  private void createPassDataGraph(IR ir, int level) {
    var passData = ir.passData();
    passData.map(
        (pass, data) -> {
          var bldr = JSVizNode.Builder.fromObject(data).level(level);
          bldr.shape("box");
          bldr.color("#d5f8ff");
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

  private void addAliasGraphScopeLabels(JSVizNode.Builder bldr, Graph.Scope scope) {
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

  private void addNode(JSVizNode node) {
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
            JSVizNode.Builder.fromObjectPlain(code)
                .shape("box")
                .level(node.level() + 1)
                .color("grey")
                .addLabelLine(code.code)
                .build();
        nodes.add(codeNode);
        createEdge(
            ir,
            code,
            "code",
            bldr -> {
              bldr.color("grey");
              bldr.dashes(true);
              return bldr;
            });
      }
    }
    if (INCLUDE_PASS_DATA) {
      if (node.object() instanceof IR ir) {
        createPassDataGraph(ir, node.level() + 1);
      }
    }
  }

  private void createEdge(
      Object from,
      Object to,
      String label,
      Function<JSVizEdge.Builder, JSVizEdge.Builder> bldrFunc) {
    assert !(from instanceof String);
    assert !(to instanceof String);
    assert !(from instanceof JSVizNode);
    assert !(to instanceof JSVizNode);
    var fromId = Utils.id(from);
    var toId = Utils.id(to);
    var fromNode = nodes.stream().filter(node -> node.id().equals(fromId)).findFirst();
    var toNode = nodes.stream().filter(node -> node.id().equals(toId)).findFirst();
    assert fromNode.isPresent()
        : "Node " + fromId + " not found. You must first create it before creating an edge from it";
    assert toNode.isPresent()
        : "Node " + toId + " not found. You must first create it before creating an edge to it";

    if (!(fromNode.get().level() < toNode.get().level())) {
      throw new AssertionError(
          "The 'from' node must be at a lower level than the 'to' node: "
              + "fromNode: "
              + fromNode.get()
              + ", toNode: "
              + toNode.get());
    }
    var edgeBldr = new JSVizEdge.Builder();
    edgeBldr.fromId(fromId).toId(toId).label(label);
    edgeBldr = bldrFunc.apply(edgeBldr);
    var edge = edgeBldr.build();
    var edgeAlreadyExists = edges.stream().anyMatch(e -> e.equals(edge));
    if (!edgeAlreadyExists) {
      edges.add(edge);
    }
  }

  private void createEdge(Object from, Object to, String label) {
    createEdge(from, to, label, Function.identity());
  }

  /**
   * Dump all the nodes and edges definitions into the JSviz format, along with the rest of the html
   * - creates a valid HTML file.
   */
  private void dumpGraph(String moduleName) {
    var title = "IR Graph for '" + moduleName + "'";
    var dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
    var generatorName = IRDumper.class.getName();
    var graphDivId = "graph";
    var nodes = indent(nodesToJS(), 8);
    var edges = indent(edgesToJS(), 8);
    var options =
        """
        {
          nodes: {
            shape: "box",
            font: {
              face: "Monospace",
              align: "left"
            },
            scaling: {
              label: {
                enabled: true,
                min: 8, // default: 14
                max: 30, // default: 30
              }
            }
          },
          edges: {
            arrows: "to"
          },
          layout: {
            hierarchical: {
              enabled: true,
              levelSeparation: 300,
              nodeSpacing: 450,
              direction: "UD",
              sortMethod: "directed" // "hubsize"
            }
          },
          physics: {
            enabled: false,
            hierarchicalRepulsion: {
              avoidOverlap: 1
            }
          }
        }
        """;
    var html =
        """
      <html lang="en">
        <head>
          <title>${title}</title>
          <script type="text/javascript" src="https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"></script>
          <style>
            .h2 {
              text-align: center;
            }
          </style>
        </head>
        <body>
          <h2>${title}</h2>
          <p>
            Generated at <b>${dateTime}</b> by <b>${generatorName}</b>.
          </p>
          <div id="${graphDivId}"></div>
          <br/>
          <script type="text/javascript">
            const options = ${options};
            const nodes = ${nodes};
            const edges = ${edges};
            var container = document.getElementById("${graphDivId}");
            var data = { nodes: nodes, edges: edges };
            var gph = new vis.Network(container, data, options);
          </script>
        </body>
      </html>
      """
            .replace("${title}", title)
            .replace("${dateTime}", dateTime)
            .replace("${nodes}", nodes)
            .replace("${options}", options)
            .replace("${edges}", edges)
            .replace("${generatorName}", generatorName)
            .replace("${graphDivId}", graphDivId);
    assert !html.contains("${") : "Not all placeholders were replaced";
    write(html);
  }

  private static String indent(String text, int indent) {
    return text.lines()
        .map(line -> " ".repeat(indent) + line)
        .collect(Collectors.joining(System.lineSeparator()));
  }

  /** Converts {@link #nodes} to a JavaScript array. */
  private String nodesToJS() {
    var sb = new StringBuilder();
    sb.append("[").append(System.lineSeparator());
    for (var node : nodes) {
      var js = node.toJSViz();
      sb.append(js).append(",").append(System.lineSeparator());
    }
    sb.append("];").append(System.lineSeparator());
    return sb.toString();
  }

  /** Converts {@link #edges} to a JavaScript array. */
  private String edgesToJS() {
    var sb = new StringBuilder();
    sb.append("[").append(System.lineSeparator());
    for (var edge : edges) {
      var js = edge.toJSViz();
      sb.append(js).append(",").append(System.lineSeparator());
    }
    sb.append("];").append(System.lineSeparator());
    return sb.toString();
  }

  private void write(String str) {
    try {
      out.write(str.getBytes());
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }

  private static RuntimeException unimpl(Object obj) {
    throw new UnsupportedOperationException(obj.getClass().getName());
  }

  /** Just a wrapper for code, we need this to be able to add the code to the graph. */
  private record Code(String code) {

    private Code(String code) {
      this.code = code;
    }
  }
}
