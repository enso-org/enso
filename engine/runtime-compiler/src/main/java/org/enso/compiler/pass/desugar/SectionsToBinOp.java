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
import org.enso.persist.Persistance;
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
              CallArgument.Specified.builder()
                  .name(Option.empty())
                  .value(rightArgName)
                  .isSynthetic(true)
                  .build();
          var rightDefArg =
              DefinitionArgument.Specified.builder()
                  .name(rightArgName.duplicate(true, true, true, false))
                  .suspended(false)
                  .build();

          if (arg.value() instanceof Name.Blank) {
            var leftArgName = freshNameSupply.newName(false, Option.empty());
            var leftCallArg =
                CallArgument.Specified.builder()
                    .name(Option.empty())
                    .value(leftArgName)
                    .isSynthetic(true)
                    .build();
            var leftDefArg =
                DefinitionArgument.Specified.builder()
                    .name(leftArgName.duplicate(true, true, true, false))
                    .suspended(false)
                    .build();
            var opCall =
                Application.Prefix.builder()
                    .function(op)
                    .arguments(cons(leftCallArg, cons(rightCallArg, nil())))
                    .hasDefaultsSuspended(false)
                    .passData(passData)
                    .diagnostics(sectionLeft.diagnostics())
                    .build();

            var rightLam =
                Function.Lambda.builder()
                    .arguments(cons(rightDefArg, nil()))
                    .bodyReference(Persistance.Reference.of(opCall))
                    .canBeTCO(true)
                    .build();

            yield Function.Lambda.builder()
                .arguments(cons(leftDefArg, nil()))
                .bodyReference(Persistance.Reference.of(rightLam))
                .canBeTCO(true)
                .location(loc)
                .build();
          } else {
            yield Application.Prefix.builder()
                .function(op)
                .arguments(cons(arg, nil()))
                .hasDefaultsSuspended(false)
                .location(loc)
                .passData(passData)
                .diagnostics(sectionLeft.diagnostics())
                .build();
          }
        }

        case Section.Sides sectionSides -> {
          var op = sectionSides.operator();
          var loc = sectionSides.location().isDefined() ? sectionSides.location().get() : null;
          var passData = sectionSides.passData();
          var leftArgName = freshNameSupply.newName(false, Option.empty());
          var leftCallArg =
              CallArgument.Specified.builder()
                  .name(Option.empty())
                  .value(leftArgName)
                  .isSynthetic(true)
                  .build();
          var leftDefArg =
              DefinitionArgument.Specified.builder()
                  .name(leftArgName.duplicate(true, true, true, false))
                  .suspended(false)
                  .build();

          var rightArgName = freshNameSupply.newName(false, Option.empty());
          var rightCallArg =
              CallArgument.Specified.builder()
                  .name(Option.empty())
                  .value(rightArgName)
                  .isSynthetic(true)
                  .build();
          var rightDefArg =
              DefinitionArgument.Specified.builder()
                  .name(rightArgName.duplicate(true, true, true, false))
                  .suspended(false)
                  .build();

          var opCall =
              Application.Prefix.builder()
                  .function(op)
                  .arguments(cons(leftCallArg, cons(rightCallArg, nil())))
                  .hasDefaultsSuspended(false)
                  .passData(passData)
                  .diagnostics(sectionSides.diagnostics())
                  .build();

          var rightLambda =
              Function.Lambda.builder()
                  .arguments(cons(rightDefArg, nil()))
                  .bodyReference(Persistance.Reference.of(opCall))
                  .canBeTCO(true)
                  .build();

          yield Function.Lambda.builder()
              .arguments(cons(leftDefArg, nil()))
              .bodyReference(Persistance.Reference.of(rightLambda))
              .canBeTCO(true)
              .location(loc)
              .build();
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
              CallArgument.Specified.builder()
                  .name(Option.empty())
                  .value(leftArgName)
                  .isSynthetic(true)
                  .build();
          var leftDefArg =
              DefinitionArgument.Specified.builder()
                  .name(leftArgName.duplicate(true, true, true, false))
                  .suspended(false)
                  .build();

          if (arg.value() instanceof Name.Blank) {
            // Note [Blanks in Sections]
            var rightArgName = freshNameSupply.newName(false, Option.empty());
            var rightCallArg =
                CallArgument.Specified.builder()
                    .name(Option.empty())
                    .value(rightArgName)
                    .isSynthetic(true)
                    .build();
            var rightDefArg =
                DefinitionArgument.Specified.builder()
                    .name(rightArgName.duplicate(true, true, true, false))
                    .suspended(false)
                    .build();

            var opCall =
                Application.Prefix.builder()
                    .function(op)
                    .arguments(cons(leftCallArg, cons(rightCallArg, nil())))
                    .hasDefaultsSuspended(false)
                    .passData(passData)
                    .diagnostics(sectionRight.diagnostics())
                    .build();

            var leftLam =
                Function.Lambda.builder()
                    .arguments(cons(leftDefArg, nil()))
                    .bodyReference(Persistance.Reference.of(opCall))
                    .canBeTCO(true)
                    .build();

            yield Function.Lambda.builder()
                .arguments(cons(rightDefArg, nil()))
                .bodyReference(Persistance.Reference.of(leftLam))
                .canBeTCO(true)
                .location(loc)
                .build();
          } else {
            var opCall =
                Application.Prefix.builder()
                    .function(op)
                    .arguments(cons(leftCallArg, cons(arg, nil())))
                    .hasDefaultsSuspended(false)
                    .passData(passData)
                    .diagnostics(sectionRight.diagnostics())
                    .build();

            yield Function.Lambda.builder()
                .arguments(cons(leftDefArg, nil()))
                .bodyReference(Persistance.Reference.of(opCall))
                .canBeTCO(true)
                .location(loc)
                .build();
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
