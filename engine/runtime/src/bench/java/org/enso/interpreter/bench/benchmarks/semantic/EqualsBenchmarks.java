package org.enso.interpreter.bench.benchmarks.semantic;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
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
 * Benchmarks for `Any.==` method. This benchmark takes two vectors as input, and compares each pair
 * of elements with `==`.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3)
@Measurement(iterations = 2)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class EqualsBenchmarks {

  private static final int primitiveVectorSize = 4_000;
  private static final int stringsVectorSize = 3_000;

  /** Maximum length of randomly generated strings. */
  private static final int maxStringSize = 20;

  /** Maximum depth of a tree (Node type). Every Node can have up to 5 children. */
  private static final int maxTreeDepth = 4;

  /** Size of the vector of trees (Node type). */
  private static final int treeVectorSize = 500;

  private Value module;
  private Value benchFunc;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var random = new Random(42);

    var ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .option(
                "enso.languageHomeOverride",
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();

    var benchmarkName = SrcUtil.findName(params);
    var codeBuilder =
        new StringBuilder(
            """
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
        """);
    // Indexes where `True` is expected. Inside the generated vectors, on a predefined indexes,
    // we put "constant" values, such that when the elements at these indexes are compared,
    // `True` is returned.
    Set<Integer> trueExpectedAt;

    switch (benchmarkName) {
      case "equalsPrimitives" -> {
        trueExpectedAt =
            Set.of(
                primitiveVectorSize / 2,
                primitiveVectorSize / 4,
                primitiveVectorSize / 8,
                primitiveVectorSize / 16,
                primitiveVectorSize / 32,
                primitiveVectorSize / 64);
        codeBuilder
            .append(
                generateVectorOfPrimitives(primitiveVectorSize, "vec1", 42, trueExpectedAt, random))
            .append("\n")
            .append(
                generateVectorOfPrimitives(primitiveVectorSize, "vec2", 42, trueExpectedAt, random))
            .append("\n");
      }
      case "equalsStrings" -> {
        trueExpectedAt = Set.of(treeVectorSize / 2, treeVectorSize / 4, treeVectorSize / 8);
        codeBuilder
            .append(
                generateVectorOfStrings(stringsVectorSize, "vec1", "AAA", trueExpectedAt, random))
            .append("\n")
            .append(
                generateVectorOfStrings(stringsVectorSize, "vec2", "AAA", trueExpectedAt, random))
            .append("\n");
      }
      case "equalsTrees" -> {
        trueExpectedAt =
            Set.of(treeVectorSize / 2, treeVectorSize / 4, treeVectorSize / 8, treeVectorSize / 16);
        codeBuilder
            .append(
                generateVectorOfTrees(
                    treeVectorSize, "vec1", maxTreeDepth, createNilNode(), trueExpectedAt, random))
            .append("\n")
            .append(
                generateVectorOfTrees(
                    treeVectorSize, "vec2", maxTreeDepth, createNilNode(), trueExpectedAt, random))
            .append("\n");
      }
      default -> throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }

    codeBuilder.append("""
        bench x = eq_vec vec1 vec2
        """);

    module = ctx.eval(SrcUtil.source(benchmarkName, codeBuilder.toString()));

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

  /**
   * Iterates over {@link #primitiveVectorSize} long vector of random generated primitive values -
   * integers, doubles, and strings
   *
   * @param blackHole
   */
  @Benchmark
  public void equalsPrimitives(Blackhole blackHole) {
    performBenchmark(blackHole);
  }

  @Benchmark
  public void equalsStrings(Blackhole blackhole) {
    performBenchmark(blackhole);
  }

  @Benchmark
  public void equalsTrees(Blackhole blackhole) {
    performBenchmark(blackhole);
  }

  private void performBenchmark(Blackhole blackhole) {
    Object res = benchFunc.execute(0);
    blackhole.consume(res);
  }

  /**
   * Generates source code for a vector of primitive values. The vector will contain integers and
   * doubles. Count of elements of these different value types is equally distributed, i.e., there
   * is exact same amount of integers and doubles. Vector is shuffled, so that there should not be a
   * long consecutive range of values of just one type.
   *
   * <p>Generates code of form {@code vecName = [...]}
   *
   * @param totalSize Total size of the generated vector.
   * @param vecName Name of the generated vector.
   * @param identityElem A primitive element considered an identity with respect to `==` operator,
   *     will be put in indexes denoted by {@code constantIdxs}
   * @param constantIdxs Indexes where {@code identityElem} will be put.
   * @param random Random number generator.
   * @return Source of the generated vector
   */
  private static String generateVectorOfPrimitives(
      int totalSize,
      String vecName,
      Object identityElem,
      Collection<Integer> constantIdxs,
      Random random) {
    var partSize = totalSize / 2;
    List<Object> primitiveValues = new ArrayList<>();
    random.ints(partSize).forEach(primitiveValues::add);
    random.doubles(partSize).forEach(primitiveValues::add);
    Collections.shuffle(primitiveValues, random);
    for (Integer constantIdx : constantIdxs) {
      primitiveValues.set(constantIdx, identityElem);
    }

    var sb = new StringBuilder();
    sb.append(vecName).append(" = [");
    for (Object primitiveValue : primitiveValues) {
      if (primitiveValue instanceof Double dbl) {
        sb.append(String.format("%f", dbl)).append(",");
      } else {
        sb.append(primitiveValue).append(",");
      }
    }
    // Replace last comma
    sb.setCharAt(sb.length() - 1, ']');
    return sb.toString();
  }

  private static String generateVectorOfStrings(
      int size,
      String vecName,
      String identityElem,
      Collection<Integer> identityIdxs,
      Random random) {
    var sb = new StringBuilder();
    sb.append(vecName).append(" = [");
    for (int i = 0; i < size; i++) {
      var stringSize = random.nextInt(maxStringSize);
      var str = identityIdxs.contains(i) ? identityElem : randomString(stringSize, random);
      sb.append("\"").append(str).append("\"").append(",");
    }
    // Replace last comma
    sb.setCharAt(sb.length() - 1, ']');
    return sb.toString();
  }

  private static Node createNilNode() {
    return new NilNode(0, null);
  }

  /**
   * Generates source code for a vector of trees (Node type), i.e., generates an expression {@code
   * vecName = [...]}.
   *
   * @param size Total size of the generated vector.
   * @param vecName How the vector should be named.
   * @param maxDepth Maximum depth of the generated tree. Note that there is no lower bound, so the
   *     generated tree can have depth 1.
   * @param identityNode A node that is considered an identity with respect to `==` operator. This
   *     node will be put on every indes of {@code constantIdxs}.
   * @param constantIdxs Indexes in the vector where {@code identityNode} should be put.
   * @param random Random number generator.
   * @return Source code for the generated tree.
   */
  private static String generateVectorOfTrees(
      int size,
      String vecName,
      int maxDepth,
      Node identityNode,
      Collection<Integer> constantIdxs,
      Random random) {
    var trees = new ArrayList<Node>();
    for (int i = 0; i < size; i++) {
      trees.add(generateTree(null, 0, random, maxDepth));
    }
    for (Integer constantIdx : constantIdxs) {
      trees.set(constantIdx, identityNode);
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
        node = new ValueNode(currDepth, parent, random.nextInt());
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
    } else if (parent != null) {
      throw new AssertionError("expected parent to be NodeWithChildren or null");
    }
    return node;
  }

  private static final String characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQESRTUVWXYZ";

  private static String randomString(int size, Random random) {
    var sb = new StringBuilder(size);
    random.ints(size, 0, characters.length()).mapToObj(characters::charAt).forEach(sb::append);
    return sb.toString();
  }

  /** A simple hierarchy of Node classes that simplifies source code generation. */
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
      String ctor =
          switch (children.size()) {
            case 1 -> "(Node.C1 ";
            case 2 -> "(Node.C2 ";
            case 3 -> "(Node.C3 ";
            case 4 -> "(Node.C4 ";
            case 5 -> "(Node.C5 ";
            default -> throw new AssertionError(
                "Unexpected number of children: " + children.size());
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
