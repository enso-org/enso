package org.enso.compiler.pass.analyse;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Type;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Comment;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.*;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.desugar.*;
import org.enso.compiler.pass.resolve.ExpressionAnnotations;
import org.enso.compiler.pass.resolve.ExpressionAnnotations$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * This pass performs tail call analysis on the Enso IR.
 *
 * <p>It is responsible for marking every single expression with whether it is in tail position.
 * This allows the code generator to correctly create the Truffle nodes. If the expression is in
 * tail position, [[TailPosition.Tail]] metadata is attached to it, otherwise, nothing is attached.
 *
 * <p>This pass requires the context to provide:
 *
 * <p>- The tail position of its expression, where relevant.
 */
public final class TailCall implements MiniPassFactory {
  public static final TailCall INSTANCE = new TailCall();
  private static final MetadataPair<TailCall> TAIL_META =
      new MetadataPair<>(INSTANCE, TailPosition.Tail);

  private TailCall() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(
            FunctionBinding$.MODULE$,
            GenerateMethodBodies$.MODULE$,
            SectionsToBinOp.INSTANCE,
            OperatorToFunction$.MODULE$,
            LambdaShorthandToLambda$.MODULE$,
            GlobalNames$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    return CollectionConverters.asScala(List.<IRProcessingPass>of()).toList();
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    var opt = inlineContext.isInTailPosition();
    if (opt.isEmpty()) {
      throw new CompilerError(
          "Information about the tail position for an inline expression "
              + "must be known by the point of tail call analysis.");
    }
    return mini(Boolean.TRUE.equals(opt.get()));
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return mini(false);
  }

  private static final Mini IN_TAIL_POS = new Mini(true);
  private static final Mini NOT_IN_TAIL_POS = new Mini(false);

  static Mini mini(boolean isInTailPos) {
    return isInTailPos ? IN_TAIL_POS : NOT_IN_TAIL_POS;
  }

  /** Expresses the tail call state of an IR Node. */
  @SuppressWarnings("unchecked")
  public static final class TailPosition implements IRPass.IRMetadata {
    public static final TailPosition Tail = new TailPosition();

    private TailPosition() {}

    /** A boolean representation of the expression's tail state. */
    public boolean isTail() {
      return true;
    }

    @Override
    public String metadataName() {
      return "TailCall.TailPosition.Tail";
    }

    @Override
    public Option duplicate() {
      return Option.apply(this);
    }

    /**
     * @inheritdoc
     */
    @Override
    public TailPosition prepareForSerialization(CompilerStub compiler) {
      return this;
    }

    /**
     * @inheritdoc
     */
    @Override
    public Option restoreFromSerialization(CompilerStub compiler) {
      return Option.apply(this);
    }
  }

  /**
   * Checks if the provided `expression` is annotated with a tail call annotation.
   *
   * @param expression the expression to check
   * @return `true` if `expression` is annotated with `@Tail_Call`, otherwise `false`
   */
  public static final boolean isTailAnnotated(Expression expression) {
    var meta = expression.passData().get(ExpressionAnnotations$.MODULE$);
    if (meta.isEmpty()) {
      return false;
    }
    var anns = (Annotations) meta.get();
    return anns.annotations().exists(a -> ExpressionAnnotations.tailCallName().equals(a.name()));
  }

  private static final class Mini extends MiniIRPass {
    private final boolean isInTailPos;

    Mini(boolean in) {
      isInTailPos = in;
    }

    @Override
    public Module transformModule(Module m) {
      m.bindings().map(this::updateModuleBinding);
      return m;
    }

    /**
     * Performs tail call analysis on a top-level definition in a module.
     *
     * @param moduleDefinition the top-level definition to analyse
     * @return `definition`, annotated with tail call information
     */
    private Void updateModuleBinding(Definition moduleDefinition) {
      switch (moduleDefinition) {
        case Method.Conversion method -> markAsTail(method);
        case Method.Explicit method -> markAsTail(method);
        case Method.Binding b -> throw new CompilerError(
            "Sugared method definitions should not occur during tail call " + "analysis.");
        case Definition.Type t -> markAsTail(t);
        case Definition.SugaredType st -> throw new CompilerError(
            "Complex type definitions should not be present during " + "tail call analysis.");
        case Comment.Documentation cd -> throw new CompilerError(
            "Documentation should not exist as an entity during tail call analysis.");
        case Type.Ascription ta -> throw new CompilerError(
            "Type signatures should not exist at the top level during " + "tail call analysis.");
        case Name.BuiltinAnnotation ba -> throw new CompilerError(
            "Annotations should already be associated by the point of " + "tail call analysis.");
        case Name.GenericAnnotation ann -> markAsTail(ann);
        default -> {}
      }
      return null;
    }

    private void markAsTailConditionally(IR ir) {
      if (isInTailPos) {
        markAsTail(ir);
      }
    }

    private Void markAsTail(IR ir) {
      ir.passData().update(TAIL_META);
      return null;
    }

    @Override
    public Expression transformExpression(Expression ir) {
      switch (ir) {
        case Literal l -> {}
        case Application.Prefix p -> {
          markAsTailConditionally(p);
          // Note [Call Argument Tail Position]
          p.arguments().foreach(a -> markAsTail(a));
        }
        case Case.Expr e -> {
          if (isInTailPos) {
            markAsTail(ir);
            // Note [Analysing Branches in Case Expressions]
            e.branches().foreach(b -> markAsTail(b));
          }
        }
        default -> markAsTailConditionally(ir);
      }
      if (!isInTailPos && isTailAnnotated(ir)) {
        ir.getDiagnostics().add(new Warning.WrongTco(ir.identifiedLocation()));
      }
      return ir;
    }

    @Override
    public Mini prepare(IR parent, Expression child) {
      var isChildTailCandidate =
          switch (parent) {
            case Module m -> true;
            case Expression e -> {
              var tailCandidates = new java.util.IdentityHashMap<IR, Boolean>();
              collectTailCandidatesExpression(e, tailCandidates);
              yield tailCandidates.containsKey(child);
            }
            default -> false;
          };
      return new Mini(isChildTailCandidate);
    }

    /**
     * Performs tail call analysis on an arbitrary expression.
     *
     * @param expression the expression to check
     * @return `expression`, annotated with tail position metadata
     */
    private void collectTailCandidatesExpression(
        Expression expression, java.util.Map<IR, Boolean> tailCandidates) {
      switch (expression) {
        case Function function -> collectTailCandicateFunction(function, tailCandidates);
        case Case caseExpr -> collectTailCandidatesCase(caseExpr, tailCandidates);
        case Application app -> collectTailCandidatesApplication(app, tailCandidates);
        case Name name -> collectTailCandidatesName(name, tailCandidates);
        case Comment c -> throw new CompilerError(
            "Comments should not be present during tail call analysis.");
        case Expression.Block b -> {
          if (isInTailPos) {
            tailCandidates.put(b.returnValue(), true);
          }
        }
        default -> {}
      }
    }

    /**
     * Performs tail call analysis on an occurrence of a name.
     *
     * @param name the name to check
     * @return `name`, annotated with tail position metadata
     */
    private void collectTailCandidatesName(Name name, java.util.Map<IR, Boolean> tailCandidates) {
      if (isInTailPos) {
        tailCandidates.put(name, true);
      }
    }

    /**
     * Performs tail call analysis on an application.
     *
     * @param application the application to check
     * @return `application`, annotated with tail position metadata
     */
    private void collectTailCandidatesApplication(
        Application application, java.util.Map<IR, Boolean> tailCandidates) {
      switch (application) {
        case Application.Prefix p -> p.arguments()
            .foreach(a -> collectTailCandidatesCallArg(a, tailCandidates));
        case Application.Force f -> {
          if (isInTailPos) {
            tailCandidates.put(f.target(), true);
          }
        }
        case Application.Sequence s -> {}
        case Application.Typeset ts -> {}
        default -> throw new CompilerError("Unexpected binary operator.");
      }
    }

    /**
     * Performs tail call analysis on a call site argument.
     *
     * @param argument the argument to check
     * @return `argument`, annotated with tail position metadata
     */
    private Void collectTailCandidatesCallArg(
        CallArgument argument, java.util.Map<IR, Boolean> tailCandidates) {
      switch (argument) {
        case CallArgument.Specified ca ->
        // Note [Call Argument Tail Position]
        tailCandidates.put(ca.value(), true);
        default -> {}
      }
      return null;
    }

    /* Note [Call Argument Tail Position]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * In order to efficiently deal with Enso's ability to suspend function
     * arguments, we behave as if all arguments to a function are passed as
     * thunks. This means that the _function_ becomes responsible for deciding
     * when to evaluate its arguments.
     *
     * Conceptually, this results in a desugaring as follows:
     *
     * ```
     * foo a b c
     * ```
     *
     * Becomes:
     *
     * ```
     * foo ({} -> a) ({} -> b) ({} -> c)
     * ```
     *
     * Quite obviously, the arguments `a`, `b` and `c` are in tail position in
     * these closures, and hence should be marked as tail.
     */

    /**
     * Performs tail call analysis on a case expression.
     *
     * @param caseExpr the case expression to check
     * @return `caseExpr`, annotated with tail position metadata
     */
    private void collectTailCandidatesCase(
        Case caseExpr, java.util.Map<IR, Boolean> tailCandidates) {
      switch (caseExpr) {
        case Case.Expr expr -> {
          if (isInTailPos) {
            // Note [Analysing Branches in Case Expressions]
            expr.branches()
                .foreach(
                    b -> {
                      tailCandidates.put(b.expression(), true);
                      return null;
                    });
          }
        }
        default -> throw new CompilerError("Unexpected case branch.");
      }
    }

    /* Note [Analysing Branches in Case Expressions]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * When performing tail call analysis on a case expression it is very
     * important to recognise that the branches of a case expression should all
     * have the same tail call state. The branches should only be marked as being
     * in tail position when the case expression _itself_ is in tail position.
     *
     * As only one branch is ever executed, it is hence safe to mark _all_
     * branches as being in tail position if the case expression is.
     */

    /**
     * Body of the function may be in tail position.
     *
     * @param function the function to check
     * @return `function`, annotated with tail position metadata
     */
    private void collectTailCandicateFunction(
        Function function, java.util.Map<IR, Boolean> tailCandidates) {
      var canBeTCO = function.canBeTCO();
      var markAsTail = (!canBeTCO && isInTailPos) || canBeTCO;
      switch (function) {
        case Function.Lambda l -> {
          if (markAsTail) {
            tailCandidates.put(l.body(), true);
          }
        }
        default -> throw new CompilerError(
            "Function sugar should not be present during tail call analysis.");
      }
    }
  }
}
