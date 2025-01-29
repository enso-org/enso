package org.enso.compiler.dump.igv;

import java.io.File;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.UUID;
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
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Comment;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Definition.Data;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;

/**
 * Implemented only for dumping the AST to IGV. Heavily inspired by the internal {@code
 * org.graalvm.compiler.truffle.compiler.TruffleAST}.
 */
final class EnsoModuleAST {
  static final ASTDumpStructure AST_DUMP_STRUCTURE = new ASTDumpStructure();

  private final ASTNode root;

  /** Underlying source file. May be null. */
  private final File srcFile;

  private final String moduleName;

  private final Map<Integer, ASTNode> nodes = new HashMap<>();

  /** List of blocks that are already built. */
  private final List<ASTBlock> blocks = new ArrayList<>();

  /** Stack of blocks that are being built. */
  private final Queue<ASTBlock.Builder> blockStack = new ArrayDeque<>();

  private final Map<UUID, Integer> nodeIds;

  private EnsoModuleAST(
      Module moduleIr, File srcFile, String moduleName, Map<UUID, Integer> nodeIds) {
    this.nodeIds = nodeIds;
    this.srcFile = srcFile;
    this.moduleName = moduleName;
    this.root = buildTree(moduleIr);
  }

  private EnsoModuleAST(Expression expr, String moduleName, Map<UUID, Integer> nodeIds) {
    this.nodeIds = nodeIds;
    this.srcFile = null;
    this.moduleName = moduleName;
    this.root = buildTree(expr);
  }

  /**
   * @param srcFile Source file for the module. May be null.
   * @param moduleName FQN of the module.
   * @param nodeIds Mapping of IR node UUIDs to sequential IDs expected by the IGV.
   */
  static EnsoModuleAST fromModuleIR(
      Module module, File srcFile, String moduleName, Map<UUID, Integer> nodeIds) {
    return new EnsoModuleAST(module, srcFile, moduleName, nodeIds);
  }

  static EnsoModuleAST fromExpressionIR(
      Expression expr, String moduleName, Map<UUID, Integer> nodeIds) {
    return new EnsoModuleAST(expr, moduleName, nodeIds);
  }

  public File getSrcFile() {
    return srcFile;
  }

  public String getModuleName() {
    return moduleName;
  }

  public List<ASTNode> getNodes() {
    return nodes.values().stream().toList();
  }

  public List<ASTBlock> getBlocks() {
    return blocks;
  }

  private void createEdge(ASTNode from, ASTNode to, String label) {
    if (!nodes.containsKey(from.getId())) {
      throw new IllegalArgumentException("Node " + from.getId() + " is not defined.");
    }
    if (!nodes.containsKey(to.getId())) {
      throw new IllegalArgumentException("Node " + to.getId() + " is not defined.");
    }
    from.addChild(to);
    from.addEdge(new ASTEdge(to, label));
  }

  private ASTNode buildTree(Module module) {
    var root = newNode(module);
    for (var i = 0; i < module.bindings().size(); i++) {
      var bindingIr = module.bindings().apply(i);
      var bindingNode = buildTree(bindingIr);
      var edgeDescr = "binding[" + i + "]";
      createEdge(root, bindingNode, edgeDescr);
    }
    for (var i = 0; i < module.imports().size(); i++) {
      var imp = module.imports().apply(i);
      var impNode = buildTree(imp);
      var edgeDescr = "import[" + i + "]";
      createEdge(root, impNode, edgeDescr);
    }
    for (var i = 0; i < module.exports().size(); i++) {
      var export = module.exports().apply(i);
      var exportNode = buildTree(export);
      var edgeDescr = "export[" + i + "]";
      createEdge(root, exportNode, edgeDescr);
    }
    return root;
  }

  private ASTNode buildTree(Import importIr) {
    return switch (importIr) {
      case Import.Module importModIr -> {
        Map<String, Object> props =
            Map.of(
                "isSynthetic", importModIr.isSynthetic(),
                "importName", importModIr.name().name(),
                "isAll", importModIr.isAll(),
                "hiddenName", importModIr.hiddenNames(),
                "rename", importModIr.rename());
        yield newNode(importModIr, props);
      }
      case Polyglot polyImport -> {
        Map<String, Object> props =
            Map.of(
                "entity", polyImport.entity(),
                "visibleName", polyImport.getVisibleName(),
                "rename", polyImport.rename());
        yield newNode(polyImport, props);
      }
      default -> throw unimpl(importIr);
    };
  }

  private ASTNode buildTree(Export exportIr) {
    return switch (exportIr) {
      case Export.Module exportModIr -> {
        Map<String, Object> props =
            Map.of(
                "isSynthetic", exportModIr.isSynthetic(),
                "exportName", exportModIr.name().name());
        yield newNode(exportModIr, props);
      }
      default -> throw unimpl(exportIr);
    };
  }

  private ASTNode buildTree(Definition definitionIr) {
    return switch (definitionIr) {
      case Method.Explicit explicitMethodIr -> {
        startBlock();
        Map<String, Object> props =
            Map.of(
                "methodName", explicitMethodIr.methodName().name(),
                "isStatic", explicitMethodIr.isStatic(),
                "isPrivate", explicitMethodIr.isPrivate(),
                "typeName", explicitMethodIr.typeName());
        var methodNode = newNode(explicitMethodIr, props);
        var methodRefNode = buildTree(explicitMethodIr.methodReference());
        var bodyNode = buildTree(explicitMethodIr.body());
        createEdge(methodNode, bodyNode, "body");
        createEdge(methodNode, methodRefNode, "methodReference");
        endBlock();
        yield methodNode;
      }
      case Method.Conversion conversionMethod -> {
        startBlock();
        Map<String, Object> props =
            Map.of(
                "methodName", conversionMethod.methodName().name(),
                "isPrivate", conversionMethod.isPrivate());
        var methodNode = newNode(conversionMethod, props);
        var methodRefNode = buildTree(conversionMethod.methodReference());
        var body = conversionMethod.body();
        var bodyNode = buildTree(body);
        createEdge(methodNode, bodyNode, "body");
        createEdge(methodNode, methodRefNode, "methodReference");
        endBlock();
        yield methodNode;
      }
      case Method.Binding binding -> {
        startBlock();
        Map<String, Object> props =
            Map.of(
                "methodName", binding.methodName().name(),
                "isPrivate", binding.isPrivate());
        var methodNode = newNode(binding, props);
        var methodRefNode = buildTree(binding.methodReference());
        createEdge(methodNode, methodRefNode, "methodReference");
        for (var i = 0; i < binding.arguments().size(); i++) {
          var arg = binding.arguments().apply(i);
          var argNode = buildTree(arg);
          createEdge(methodNode, argNode, "arg[" + i + "]");
        }
        var body = binding.body();
        var bodyNode = buildTree(body);
        createEdge(methodNode, bodyNode, "body");
        endBlock();
        yield methodNode;
      }
      case Definition.Type type -> {
        startBlock();
        Map<String, Object> props = Map.of("typeName", type.name().name());
        var typeNode = newNode(type, props);
        for (var i = 0; i < type.members().size(); i++) {
          var member = type.members().apply(i);
          var memberNode = buildTree(member);
          createEdge(typeNode, memberNode, "member[" + i + "]");
        }
        endBlock();
        yield typeNode;
      }
      case Definition.SugaredType type -> {
        Map<String, Object> props = Map.of("typeName", type.name().name());
        var node = newNode(type, props);
        for (var i = 0; i < type.arguments().size(); i++) {
          var arg = type.arguments().apply(i);
          var argNode = buildTree(arg);
          createEdge(node, argNode, "arg[" + i + "]");
        }
        yield node;
      }
      case Name.GenericAnnotation genericAnnotation -> {
        Map<String, Object> props =
            Map.of(
                "annotationName", genericAnnotation.name(),
                "isMethod", genericAnnotation.isMethod());
        var anotNode = newNode(genericAnnotation, props);
        var expr = genericAnnotation.expression();
        var exprNode = buildTree(expr);
        createEdge(anotNode, exprNode, "expression");
        yield anotNode;
      }
      case Name.BuiltinAnnotation builtinAnnotation -> {
        Map<String, Object> props = Map.of("annotationName", builtinAnnotation.name());
        yield newNode(builtinAnnotation, props);
      }
      case Type.Ascription ascription -> {
        var ascrNode = newNode(ascription);
        var typed = ascription.typed();
        var typedNode = buildTree(typed);
        createEdge(ascrNode, typedNode, "typed");
        var signature = ascription.signature();
        var signatureNode = buildTree(signature);
        createEdge(ascrNode, signatureNode, "signature");
        yield ascrNode;
      }
      case Comment.Documentation doc -> {
        Map<String, Object> props = Map.of("doc", doc.doc());
        yield newNode(doc, props);
      }
      default -> throw unimpl(definitionIr);
    };
  }

  private ASTNode buildTree(Data atomCons) {
    Map<String, Object> props = Map.of("consName", atomCons.name().name());
    var consNode = newNode(atomCons, props);
    for (var i = 0; i < atomCons.arguments().size(); i++) {
      var arg = atomCons.arguments().apply(i);
      var argNode = buildTree(arg);
      createEdge(consNode, argNode, "arg[" + i + "]");
    }
    return consNode;
  }

  private ASTNode buildTree(Expression expression) {
    return switch (expression) {
      case Expression.Block block -> {
        var blockNode = newNode(block);
        for (var i = 0; i < block.expressions().size(); i++) {
          var expr = block.expressions().apply(i);
          var exprNode = buildTree(expr);
          createEdge(blockNode, exprNode, "expression[" + i + "]");
        }
        var retValNode = buildTree(block.returnValue());
        createEdge(blockNode, retValNode, "returnValue");
        yield blockNode;
      }
      case Case.Expr caseExpr -> {
        Map<String, Object> props = Map.of("isNested", caseExpr.isNested());
        var caseNode = newNode(caseExpr, props);
        var scrutineeNode = buildTree(caseExpr.scrutinee());
        createEdge(caseNode, scrutineeNode, "scrutinee");
        for (var i = 0; i < caseExpr.branches().size(); i++) {
          var branch = caseExpr.branches().apply(i);
          var branchNode = buildTree(branch);
          createEdge(caseNode, branchNode, "branch[" + i + "]");
        }
        yield caseNode;
      }
      case Case.Branch caseBranch -> {
        Map<String, Object> props = Map.of("isTerminal", caseBranch.terminalBranch());
        var branchNode = newNode(caseBranch, props);
        var patternNode = buildTree(caseBranch.pattern());
        createEdge(branchNode, patternNode, "pattern");
        var exprNode = buildTree(caseBranch.expression());
        createEdge(branchNode, exprNode, "expression");
        yield branchNode;
      }
      case Application.Prefix prefixApp -> {
        Map<String, Object> props =
            Map.of("hasDefaultsSuspended", prefixApp.hasDefaultsSuspended());
        var prefixAppNode = newNode(prefixApp, props);
        var funcNode = buildTree(prefixApp.function());
        createEdge(prefixAppNode, funcNode, "function");
        for (var i = 0; i < prefixApp.arguments().size(); i++) {
          var arg = prefixApp.arguments().apply(i);
          var argNode = buildTree(arg);
          createEdge(prefixAppNode, argNode, "arg[" + i + "]");
        }
        yield prefixAppNode;
      }
      case Function.Lambda lambda -> {
        var lambdaNode = newNode(lambda);
        var bodyNode = buildTree(lambda.body());
        createEdge(lambdaNode, bodyNode, "body");
        for (var i = 0; i < lambda.arguments().size(); i++) {
          var arg = lambda.arguments().apply(i);
          var argNode = buildTree(arg);
          createEdge(lambdaNode, argNode, "arg[" + i + "]");
        }
        yield lambdaNode;
      }
      case Expression.Binding exprBinding -> {
        Map<String, Object> props = Map.of("bindingName", exprBinding.name().name());
        var node = newNode(exprBinding, props);
        var exprNode = buildTree(exprBinding.expression());
        createEdge(node, exprNode, "expression");
        yield node;
      }
      case Number number -> {
        Map<String, Object> props = Map.of("value", number.value());
        yield newNode(number, props);
      }
      case Text text -> {
        Map<String, Object> props = Map.of("text", text.text());
        yield newNode(text, props);
      }
      case Name.Literal literal -> {
        Map<String, Object> props =
            Map.of(
                "literalName", literal.name(),
                "isMethod", literal.isMethod(),
                "originalName", literal.originalName());
        yield newNode(literal, props);
      }
      case Name.Qualified qualName -> {
        Map<String, Object> props =
            Map.of(
                "qualName", qualName.name(),
                "isMethod", qualName.isMethod(),
                "parts", qualName.parts());
        yield newNode(qualName, props);
      }
      case Name.MethodReference methodRef -> {
        Map<String, Object> props =
            Map.of(
                "methodName", methodRef.methodName().name(),
                "typePointer", methodRef.typePointer());
        yield newNode(methodRef, props);
      }
      default -> newNode(expression);
    };
  }

  private ASTNode buildTree(Pattern pattern) {
    return switch (pattern) {
      case Pattern.Constructor constrPat -> {
        Map<String, Object> props = Map.of("constructor", constrPat.constructor().name());
        var node = newNode(constrPat, props);
        for (var i = 0; i < constrPat.fields().size(); i++) {
          var field = constrPat.fields().apply(i);
          var fieldNode = buildTree(field);
          createEdge(node, fieldNode, "field[" + i + "]");
        }
        yield node;
      }
      case Pattern.Type tp -> {
        var node = newNode(tp);
        var nameNode = buildTree(tp.name());
        var tpeNode = buildTree(tp.tpe());
        createEdge(node, nameNode, "name");
        createEdge(node, tpeNode, "tpe");
        yield node;
      }
      case Pattern.Literal litPat -> {
        var node = newNode(litPat);
        var litNode = buildTree(litPat.literal());
        createEdge(node, litNode, "literal");
        yield node;
      }
      case Pattern.Name name -> {
        Map<String, Object> props = Map.of("patternName", name.name().name());
        yield newNode(name, props);
      }
      case Pattern.Documentation doc -> {
        Map<String, Object> props = Map.of("doc", doc.doc());
        yield newNode(doc, props);
      }
      default -> throw unimpl(pattern);
    };
  }

  private ASTNode buildTree(CallArgument argument) {
    return switch (argument) {
      case CallArgument.Specified specifiedArg -> {
        Map<String, Object> props = Map.of("argName", specifiedArg.name());
        var node = newNode(specifiedArg, props);
        var valueNode = buildTree(specifiedArg.value());
        createEdge(node, valueNode, "value");
        yield node;
      }
      default -> throw unimpl(argument);
    };
  }

  private ASTNode buildTree(DefinitionArgument argument) {
    return switch (argument) {
      case DefinitionArgument.Specified specifiedArg -> {
        Map<String, Object> props =
            Map.of(
                "argName", specifiedArg.name().name(),
                "suspended", specifiedArg.suspended());
        var node = newNode(specifiedArg, props);
        if (specifiedArg.ascribedType().isDefined()) {
          var ascribedTypeNode = buildTree(specifiedArg.ascribedType().get());
          createEdge(node, ascribedTypeNode, "ascribedType");
        }
        if (specifiedArg.defaultValue().isDefined()) {
          var defaultValueNode = buildTree(specifiedArg.defaultValue().get());
          createEdge(node, defaultValueNode, "defaultValue");
        }
        yield node;
      }
      default -> throw unimpl(argument);
    };
  }

  private ASTNode newNode(IR ir, Map<String, Object> props) {
    ASTNode.Builder bldr = ASTNode.Builder.fromIr(ir, srcFile);
    var nodeId = nodeIds.get(ir.getId());
    if (nodeId == null) {
      var lastSeqId = nodeIds.size();
      nodeIds.put(ir.getId(), lastSeqId);
      nodeId = lastSeqId;
    }
    bldr.id(nodeId);
    props.forEach(bldr::property);
    var node = bldr.build();
    assert !nodes.containsKey(node.getId());
    nodes.put(node.getId(), node);
    if (currentBlockBldr() != null) {
      currentBlockBldr().addNode(node);
    }
    return node;
  }

  private ASTNode newNode(IR ir) {
    return newNode(ir, Map.of());
  }

  private void startBlock() {
    var blockBldr = ASTBlock.Builder.fromId(blocks.size());
    blockStack.add(blockBldr);
  }

  private ASTBlock.Builder currentBlockBldr() {
    return blockStack.peek();
  }

  private void endBlock() {
    assert !blockStack.isEmpty();
    var block = blockStack.remove().build();
    var parentBlock = blockStack.peek();
    if (parentBlock != null) {
      parentBlock.addSuccessor(block);
    }
    blocks.add(block);
  }

  private static RuntimeException unimpl(Object obj) {
    throw new UnsupportedOperationException(
        "Not implemented IR processing for class: " + obj.getClass().getName());
  }
}
