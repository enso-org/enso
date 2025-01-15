package org.enso.compiler.pass.desugar;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Section;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.CachePreferenceAnalysis$;
import org.enso.compiler.pass.analyse.DataflowAnalysis$;
import org.enso.compiler.pass.analyse.DemandAnalysis$;
import org.enso.compiler.pass.analyse.TailCall;
import org.enso.compiler.pass.lint.UnusedBindings$;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

public final class SectionsToBinOp implements MiniPassFactory {

  public static final SectionsToBinOp INSTANCE = new SectionsToBinOp();

  private SectionsToBinOp() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes = List.of(GenerateMethodBodies$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    List<IRProcessingPass> passes =
        List.of(
            AliasAnalysis$.MODULE$,
            CachePreferenceAnalysis$.MODULE$,
            DataflowAnalysis$.MODULE$,
            DemandAnalysis$.MODULE$,
            TailCall.INSTANCE,
            UnusedBindings$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    var ctx = InlineContext.fromModuleContext(moduleContext);
    return new Mini(ctx);
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return new Mini(inlineContext);
  }

  private static final class Mini extends MiniIRPass {

    private final InlineContext ctx;

    private Mini(InlineContext ctx) {
      this.ctx = ctx;
    }

    public Expression transformExpression(Expression ir) {
      var freshNameSupply = ctx.freshNameSupply().get();
      return switch (ir) {
        case Section.Left sectionLeft -> {
          var arg = sectionLeft.arg();
          var op = sectionLeft.operator();
          var loc = sectionLeft.location().isDefined() ? sectionLeft.location().get() : null;
          var passData = sectionLeft.passData();
          var rightArgName = freshNameSupply.newName(false, Option.empty());
          var rightCallArg =
              new CallArgument.Specified(Option.empty(), rightArgName, true, null, meta());
          var rightDefArg =
              new DefinitionArgument.Specified(
                  rightArgName.duplicate(true, true, true, false),
                  Option.empty(),
                  Option.empty(),
                  false,
                  null,
                  meta());

          if (arg.value() instanceof Name.Blank) {
            var leftArgName = freshNameSupply.newName(false, Option.empty());
            var leftCallArg =
                new CallArgument.Specified(Option.empty(), leftArgName, true, null, meta());
            var leftDefArg =
                new DefinitionArgument.Specified(
                    leftArgName.duplicate(true, true, true, false),
                    Option.empty(),
                    Option.empty(),
                    false,
                    null,
                    meta());
            var opCall =
                new Application.Prefix(
                    op,
                    cons(leftCallArg, cons(rightCallArg, nil())),
                    false,
                    null,
                    passData,
                    sectionLeft.diagnostics());

            var rightLam =
                new Function.Lambda(cons(rightDefArg, nil()), opCall, null, true, meta());

            yield new Function.Lambda(cons(leftDefArg, nil()), rightLam, loc, true, meta());
          } else {
            yield new Application.Prefix(
                op, cons(arg, nil()), false, loc, passData, sectionLeft.diagnostics());
          }
        }

        case Section.Sides sectionSides -> {
          var op = sectionSides.operator();
          var loc = sectionSides.location().isDefined() ? sectionSides.location().get() : null;
          var passData = sectionSides.passData();
          var leftArgName = freshNameSupply.newName(false, Option.empty());
          var leftCallArg =
              new CallArgument.Specified(Option.empty(), leftArgName, true, null, meta());
          var leftDefArg =
              new DefinitionArgument.Specified(
                  leftArgName.duplicate(true, true, true, false),
                  Option.empty(),
                  Option.empty(),
                  false,
                  null,
                  meta());

          var rightArgName = freshNameSupply.newName(false, Option.empty());
          var rightCallArg =
              new CallArgument.Specified(Option.empty(), rightArgName, true, null, meta());
          var rightDefArg =
              new DefinitionArgument.Specified(
                  rightArgName.duplicate(true, true, true, false),
                  Option.empty(),
                  Option.empty(),
                  false,
                  null,
                  meta());

          var opCall =
              new Application.Prefix(
                  op,
                  cons(leftCallArg, cons(rightCallArg, nil())),
                  false,
                  null,
                  passData,
                  sectionSides.diagnostics());

          var rightLambda =
              new Function.Lambda(cons(rightDefArg, nil()), opCall, null, true, meta());

          yield new Function.Lambda(cons(leftDefArg, nil()), rightLambda, loc, true, meta());
        }

          /* Note [Blanks in Sections]
           * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           * While the naiive compositional translation of `(- _)` first translates
           * the section into a function applying `-` to two arguments, one of which
           * is a blank, the compositional nature of the blanks translation actually
           * works against us here.
           *
           * As the `LambdaShorthandToLambda` pass can only operate on the
           * application with the blanks, it can't know to push the blank outside
           * that application chain. To that end, we have to handle this case
           * specially here instead. What we want it to translate to is as follows:
           *
           * `(- _)` == `x -> (- x)` == `x -> y -> y - x`
           *
           * We implement this special case here.
           *
           * The same is true of left sections.
           */

        case Section.Right sectionRight -> {
          var arg = sectionRight.arg();
          var op = sectionRight.operator();
          var loc = sectionRight.location().isDefined() ? sectionRight.location().get() : null;
          var passData = sectionRight.passData();
          var leftArgName = freshNameSupply.newName(false, Option.empty());
          var leftCallArg =
              new CallArgument.Specified(Option.empty(), leftArgName, true, null, meta());
          var leftDefArg =
              new DefinitionArgument.Specified(
                  leftArgName.duplicate(true, true, true, false),
                  Option.empty(),
                  Option.empty(),
                  false,
                  null,
                  meta());

          if (arg.value() instanceof Name.Blank) {
            // Note [Blanks in Sections]
            var rightArgName = freshNameSupply.newName(false, Option.empty());
            var rightCallArg =
                new CallArgument.Specified(Option.empty(), rightArgName, true, null, meta());
            var rightDefArg =
                new DefinitionArgument.Specified(
                    rightArgName.duplicate(true, true, true, false),
                    Option.empty(),
                    Option.empty(),
                    false,
                    null,
                    meta());

            var opCall =
                new Application.Prefix(
                    op,
                    cons(leftCallArg, cons(rightCallArg, nil())),
                    false,
                    null,
                    passData,
                    sectionRight.diagnostics());

            var leftLam = new Function.Lambda(cons(leftDefArg, nil()), opCall, null, true, meta());

            yield new Function.Lambda(cons(rightDefArg, nil()), leftLam, loc, true, meta());
          } else {
            var opCall =
                new Application.Prefix(
                    op,
                    cons(leftCallArg, cons(arg, nil())),
                    false,
                    null,
                    passData,
                    sectionRight.diagnostics());

            yield new Function.Lambda(cons(leftDefArg, nil()), opCall, loc, true, meta());
          }
        }
        default -> ir;
      };
    }

    private static MetadataStorage meta() {
      return new MetadataStorage();
    }

    @SuppressWarnings("unchecked")
    private static <T> scala.collection.immutable.List<T> nil() {
      return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
    }

    private static <T> scala.collection.immutable.List<T> cons(
        T head, scala.collection.immutable.List<T> tail) {
      return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
    }
  }
}
