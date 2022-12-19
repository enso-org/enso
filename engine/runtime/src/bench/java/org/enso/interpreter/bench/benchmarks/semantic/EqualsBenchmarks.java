package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.enso.polyglot.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Benchmarks for `Any.==` method.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3)
@Measurement(iterations = 2)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class EqualsBenchmarks {

  private static final int primitiveVectorSize = 4_000;
  private static final int maxStringSize = 20;
  private static final int maxTreeDepth = 4;
  private static final int treeVectorSize = 500;
  private Set<Integer> trueExpectedAt;
  private Value module;
  private Value benchFunc;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var random = new Random(42);

    var ctx = Context.newBuilder()
        .allowExperimentalOptions(true)
        .logHandler(new ByteArrayOutputStream())
        .allowIO(true)
        .allowAllAccess(true)
        .option(
            "enso.languageHomeOverride",
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build();

    var benchmarkName = params.getBenchmark().replaceFirst(".*\\.", "");
    var codeBuilder = new StringBuilder("""
        import Standard.Base.Data.Range.Extensions
        
        type Node
            C1 f1
            C2 f1 f2
            C3 f1 f2 f3
            C4 f1 f2 f3 f4
            C5 f1 f2 f3 f4 f5
            Nil
            Value value
        
        eq_vec vec1 vec2 =
            (0.up_to vec1.length).map idx->
                (vec1.at idx) == (vec2.at idx)
        
        eq x y = x == y
        """
    );
    switch (benchmarkName) {
      case "equalsPrimitives" -> {
        trueExpectedAt = Set.of(
            primitiveVectorSize / 2,
            primitiveVectorSize / 4,
            primitiveVectorSize / 8,
            primitiveVectorSize / 16,
            primitiveVectorSize / 32,
            primitiveVectorSize / 64
        );
        codeBuilder
            .append(
                generateVectorOfPrimitives(primitiveVectorSize, "vec1", 42, trueExpectedAt, random)
            )
            .append("\n");
        codeBuilder
            .append(
                generateVectorOfPrimitives(primitiveVectorSize, "vec2", 42, trueExpectedAt, random)
            )
            .append("\n");
      }
      case "equalsTrees" -> {
        trueExpectedAt = Set.of(
            treeVectorSize / 2,
            treeVectorSize / 4,
            treeVectorSize / 8,
            treeVectorSize / 16,
            treeVectorSize / 32,
            treeVectorSize / 64
        );
        codeBuilder
            .append(
                generateVectorOfTrees(treeVectorSize, "vec1", maxTreeDepth, createNilNode(), trueExpectedAt, random)
            )
            .append("\n");
        codeBuilder
            .append(
                generateVectorOfTrees(treeVectorSize, "vec2", maxTreeDepth, createNilNode(), trueExpectedAt, random)
            )
            .append("\n");
      }
      default ->
        throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }

    codeBuilder.append("""
        bench x = eq_vec vec1 vec2
        """);

    module = ctx.eval(
        SrcUtil.source(benchmarkName, codeBuilder.toString())
    );

    benchFunc = module.invokeMember(Module.EVAL_EXPRESSION, "bench");

    // Sanity check - elements in `trueExpectedAt` should be the same
    var eqFunc = module.invokeMember(Module.EVAL_EXPRESSION, "eq");
    var vec1 = module.invokeMember(Module.EVAL_EXPRESSION, "vec1");
    var vec2 = module.invokeMember(Module.EVAL_EXPRESSION, "vec2");
    for (Integer idx : trueExpectedAt) {
      var result = eqFunc.execute(vec1.getArrayElement(idx), vec2.getArrayElement(idx));
      if (!result.asBoolean()) {
        throw new AssertionError("Elements of input vectors at " + idx + " should be the same.");
      }
    }
  }

  @Benchmark
  public void equalsPrimitives(Blackhole blackHole) {
    performBenchmark(blackHole);
  }

  @Benchmark
  public void equalsTrees(Blackhole blackhole) {
    performBenchmark(blackhole);
  }

  private void performBenchmark(Blackhole blackhole) {
    Object res = benchFunc.execute(0);
    blackhole.consume(res);
  }

  private static String generateVectorOfPrimitives(int totalSize, String vecName, Object constantElem, Collection<Integer> constantIdxs, Random random) {
    var partSize = totalSize / 3;
    List<Object> primitiveValues = new ArrayList<>();
    random.ints(partSize).forEach(primitiveValues::add);
    random.doubles(partSize).forEach(primitiveValues::add);
    for (int i = 0; i < partSize; i++) {
      var stringSize = random.nextInt(maxStringSize);
      primitiveValues.add(
          randomString(stringSize, random)
      );
    }
    Collections.shuffle(primitiveValues, random);
    for (Integer constantIdx : constantIdxs) {
      primitiveValues.set(constantIdx, constantElem);
    }

    var sb = new StringBuilder();
    sb.append(vecName).append(" = [");
    for (Object primitiveValue : primitiveValues) {
      if (primitiveValue instanceof String str) {
        sb.append("\"").append(str).append("\"").append(",");
      } else if (primitiveValue instanceof Double dbl) {
        sb.append(String.format("%f", dbl)).append(",");
      } else {
        sb.append(primitiveValue).append(",");
      }
    }
    // Replace last comma
    sb.setCharAt(sb.length() - 1, ']');
    return sb.toString();
  }

  private static Node createNilNode() {
    return new NilNode(0, null);
  }

  private static String generateVectorOfTrees(int size, String vecName, int maxDepth, Node constantNode, Collection<Integer> constantIdxs, Random random) {
    var trees = new ArrayList<Node>();
    for (int i = 0; i < size; i++) {
      trees.add(
          generateTree(null, 0, random, maxDepth)
      );
    }
    for (Integer constantIdx : constantIdxs) {
      trees.set(constantIdx, constantNode);
    }

    var sb = new StringBuilder();
    sb.append(vecName).append(" = [");
    for (Node tree : trees) {
      sb.append(tree.createSource()).append(",");
    }
    sb.setCharAt(sb.length() - 1, ']');
    return sb.toString();
  }

  private static Node generateTree(Node parent, int currDepth, Random random, int maxDepth) {
    Node node;
    if (currDepth == maxDepth) {
      node = new NilNode(currDepth, parent);
    } else {
      if (random.nextBoolean() && currDepth > 0) {
        node = new ValueNode(
            currDepth,
            parent,
            random.nextInt()
        );
      } else {
        node = new NodeWithChildren(currDepth, parent);
        // childCount is between 1..5
        var childCount = Math.max(random.nextInt(5), 1);
        for (int i = 0; i < childCount; i++) {
          generateTree(node, currDepth + 1, random, maxDepth);
        }
      }
    }
    if (parent instanceof NodeWithChildren parentWithChildren) {
      parentWithChildren.addChild(node);
    } else if (parent != null){
      throw new AssertionError("expected parent to be NodeWithChildren or null");
    }
    return node;
  }

  private static final String characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQESRTUVWXYZ";
  private static String randomString(int size, Random random) {
    var sb = new StringBuilder(size);
    random
        .ints(size, 0, characters.length())
        .mapToObj(characters::charAt)
        .forEach(sb::append);
    return sb.toString();
  }


  abstract static class Node {
    final int depth;
    final Node parent;

    Node(int depth, Node parent) {
      this.depth = depth;
      this.parent = parent;
    }

    abstract String createSource();
  }

  private static final class ValueNode extends Node {
    final int value;
    ValueNode(int depth, Node parent, int value) {
      super(depth, parent);
      this.value = value;
    }

    @Override
    public String createSource() {
      return "(Node.Value " + value + ")";
    }
  }

  private static final class NilNode extends Node {

    NilNode(int depth, Node parent) {
      super(depth, parent);
    }

    @Override
    String createSource() {
      return "Node.Nil";
    }
  }

  private static final class NodeWithChildren extends Node {
    private List<Node> children = new ArrayList<>();

    NodeWithChildren(int depth, Node parent) {
      super(depth, parent);
    }

    void addChild(Node node) {
      children.add(node);
    }

    @Override
    public String createSource() {
      String ctor = switch(children.size()) {
        case 1 -> "(Node.C1 ";
        case 2 -> "(Node.C2 ";
        case 3 -> "(Node.C3 ";
        case 4 -> "(Node.C4 ";
        case 5 -> "(Node.C5 ";
        default -> throw new AssertionError("Unexpected number of children: " + children.size());
      };
      var sb = new StringBuilder();
      sb.append(ctor);
      for (Node child : children) {
        sb.append(child.createSource()).append(" ");
      }
      sb.append(")");
      return sb.toString();
    }
  }
}
