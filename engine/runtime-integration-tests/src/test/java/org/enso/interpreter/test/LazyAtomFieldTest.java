package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.stream.Collectors;
import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class LazyAtomFieldTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Before
  public void resetOut() {
    ctxRule.resetOut();
  }

  @Test
  public void evaluation() throws Exception {
    final String code =
        """
        from Standard.Base import IO

        type Lazy
            LazyValue ~x ~y

            say self w = "Hello " + w.to_text

            meaning self =
                IO.println "Computing meaning"
                v = self.x * self.y
                IO.println "Computed meaning"
                v

        meanings =
            compute_x =
                IO.println "Computing x"
                v = 6
                IO.println "Computing x done"
                v

            compute_y =
                IO.println "Computing y"
                v = 7
                IO.println "Computing y done"
                v

            IO.println "Start"
            l = Lazy.LazyValue compute_x compute_y
            IO.println "Lazy value ready"
            IO.println <| l.say "World!"
            IO.println l.meaning
            IO.println <| l.say "Again!"
            IO.println l.meaning
            l.meaning
        """;
    var meanings = evalCode(code, "meanings");
    assertEquals(42, meanings.asInt());

    String log = ctxRule.getOut();
    var lazyReadyAndThen =
        log.lines().dropWhile(l -> l.contains("Lazy value ready")).collect(Collectors.toList());
    var computingX = lazyReadyAndThen.stream().filter(l -> l.contains("Computing x done")).count();
    assertEquals(log, 1, computingX);
    var computingY = lazyReadyAndThen.stream().filter(l -> l.contains("Computing y done")).count();
    assertEquals(log, 1, computingY);
    var hellos = lazyReadyAndThen.stream().filter(l -> l.startsWith("Hello")).count();
    assertEquals(log, 2, hellos);
  }

  @Test
  public void testInfiniteListGenerator() throws Exception {
    final String code =
        """
        import Standard.Base.IO

        type Lazy
            Nil
            Cons ~x ~xs

            take self n = if n == 0 then Lazy.Nil else case self of
                Lazy.Nil -> Lazy.Nil
                Lazy.Cons x xs -> Lazy.Cons x (xs.take n-1)

            sum self acc = case self of
                Lazy.Nil -> acc
                Lazy.Cons x xs -> @Tail_Call xs.sum acc+x

            generator n = Lazy.Cons n (Lazy.generator n+1)

        both n =
            g = Lazy.generator 1
            # IO.println "Generator is computed"
            t = g.take n
            # IO.println "Generator is taken"
            t . sum 0
        """;

    var both = evalCode(code, "both");
    var sum = both.execute(100);
    String log = ctxRule.getOut();
    assertEquals(log, 5050, sum.asLong());
  }

  @Test
  public void fourAtomIntFields() throws Exception {
    checkNumHolder(
        """
        type Num
            Holder a b c ~num

            new  = Num.Holder 1 2 3 (R.new.nextInt)
        """);
  }

  @Test
  public void fourAtomObjectFields() throws Exception {
    checkNumHolder(
        """
        type Num
            Holder a b c ~num

            new  = Num.Holder "a" "b" "c" (R.new.nextInt)
        """);
  }

  @Test
  public void fiveAtomIntFields() throws Exception {
    checkNumHolder(
        """
        type Num
            Holder a b c d ~num

            new  = Num.Holder 1 2 3 4 (R.new.nextInt)
        """);
  }

  @Test
  public void fiveAtomObjectFields() throws Exception {
    checkNumHolder(
        """
        type Num
            Holder a b c d ~num

            new  = Num.Holder "a" "b" "c" "d" (R.new.nextInt)
        """);
  }

  @Test
  public void toTextOnAtomWithLazyField() throws URISyntaxException {
    var res =
        evalCode(
            """
            from Standard.Base.Any import all

            type Generator
                Value n ~next

            natural =
                gen n = Generator.Value n (gen n+1)
                gen 2

            main _ =
                two = natural
                two.to_text
            """,
            "main");
    assertTrue(res.isString());
  }

  @Test
  public void lazyAtomFieldIsNotEvaluated_InStructuralPatternMatch() {
    var code =
        """
        from Standard.Base import Nothing, IO

        type My_Ref
            Lazy ~lazy
            Eager eager

        main =
            v1 = My_Ref.Lazy <|
                IO.println "Computing v1"
                42

            case v1 of
                My_Ref.Eager e -> e
                My_Ref.Lazy _ -> Nothing
        """;
    var res = ctxRule.evalModule(code);
    assertThat(res.isNull(), is(true));
    assertThat(
        "Lazy field is not evaluated in pattern match",
        ctxRule.getStdOut(),
        not(containsString("Computing v1")));
  }

  @Test
  public void lazyAtomFieldIsNotEvaluated_InStructuralPatternMatch_WithBoundedField() {
    var code =
        """
        from Standard.Base import Nothing, IO, False

        type My_Ref
            Lazy ~lazy
            Eager eager

        main =
            v2 = My_Ref.Lazy <|
                IO.println "Computing v2"
                42

            should_compute = False

            case v2 of
                My_Ref.Eager e -> e
                My_Ref.Lazy l ->
                    if should_compute then l else Nothing
        """;
    var res = ctxRule.evalModule(code);
    assertThat(res.isNull(), is(true));
    assertThat(
        "Lazy field is not evaluated in pattern match with bounded field",
        ctxRule.getStdOut(),
        not(containsString("Computing v2")));
  }

  private void checkNumHolder(String typeDefinition) throws Exception {
    var code =
        "polyglot java import java.util.Random as R\n"
            + typeDefinition
            + """

            create ignore =
              fbl = Num.new
              f = fbl.num
              n = fbl.num
              [ f, n ]
            """;
    var create = evalCode(code, "create");
    var tupple = create.execute(0);

    assertEquals("Two values", 2, tupple.getArraySize());
    var first = tupple.getArrayElement(0).asInt();
    var second = tupple.getArrayElement(1).asInt();

    assertEquals("Both numbers are the same", first, second);
  }

  private Value evalCode(final String code, final String methodName) throws URISyntaxException {
    final var testName = "test.enso";
    final URI testUri = new URI("memory://" + testName);
    final Source src = Source.newBuilder("enso", code, testName).uri(testUri).buildLiteral();
    var module = ctxRule.eval(src);
    return module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, methodName);
  }
}
