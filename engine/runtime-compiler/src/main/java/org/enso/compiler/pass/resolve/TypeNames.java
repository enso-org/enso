package org.enso.compiler.pass.resolve;

import java.util.List;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.type.Set;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.Resolution;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.scala.wrapper.ScalaConversions;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.util.Either;
import scala.util.Left;
import scala.util.Right;

/** Resolves and desugars referent name occurrences in type positions. */
public final class TypeNames implements MiniPassFactory {

  public static final TypeNames INSTANCE = new TypeNames();

  private TypeNames() {}

  @Override
  public Seq<IRPass> precursorPasses() {
    return ScalaConversions.seq(List.of(BindingAnalysis$.MODULE$));
  }

  @Override
  public Seq<IRPass> invalidatedPasses() {
    return ScalaConversions.seq(List.of());
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return new Mini(moduleContext);
  }

  /** This pass can only process whole modules */
  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  private static final class Mini extends MiniIRPass {

    private final ModuleContext moduleContext;
    private final BindingsMap bindingsMap;
    private final SelfTypeInfo selfTypeInfo;

    /** Initial constructor for processing the whole module */
    private Mini(ModuleContext mc) {
      this.moduleContext = mc;
      this.bindingsMap = mc.bindingsAnalysis();
      this.selfTypeInfo = SelfTypeInfo.empty;
    }

    private Mini(ModuleContext mc, BindingsMap map, SelfTypeInfo info) {
      this.moduleContext = mc;
      this.bindingsMap = map;
      this.selfTypeInfo = info;
    }

    /**
     * Copy constructor.
     *
     * @param info new info to use
     * @return new instance of mini pass
     */
    private Mini withSelfType(SelfTypeInfo info) {
      return new Mini(moduleContext, bindingsMap, info);
    }

    /**
     * Descend down to children. Detect definitions of type and methods. Record self type by copying
     * itself.
     *
     * @param parent process only bindings - direct children of main module
     * @param child expression to check
     * @return itself or own copy
     */
    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      if (!(parent instanceof Module)) {
        return this;
      }
      return switch (child) {
        case Definition.Type t -> {
          var selfType = SelfTypeInfo.fromTypeDefinition(t);
          yield withSelfType(selfType);
        }
        case Method.Explicit m -> {
          var selfType = SelfTypeInfo.fromMethodReference(m.methodReference());
          yield withSelfType(selfType);
        }
        default -> this;
      };
    }

    @Override
    public Expression transformExpression(Expression ir) {
      if (ir instanceof Function.Lambda fn) {
        ir =
            fn.copyWithArguments(
                fn.arguments().map(a -> doResolveType(selfTypeInfo, bindingsMap, a)));
      }
      var expr = doResolveType(selfTypeInfo, bindingsMap, ir);
      return expr;
    }

    @Override
    public Module transformModule(Module ir) {
      /*
      var withResolvedArguments = switch (mapped) {
        case Definition.Type typ -> {
          typ.members().foreach(m ->
            m.arguments().foreach(a ->
              doResolveType(
                SelfTypeInfo.fromTypeDefinition(typ),
                bindingsMap,
                a
              )
            )
          );
          yield typ;
        }
        case Expression x -> x;
      };
      doResolveType(selfTypeInfo, bindingsMap, withResolvedArguments);
             */
      return ir;
    }
  }

  private record SelfTypeInfo(
      Option<BindingsMap.ResolvedType> selfType, scala.collection.immutable.List<Name> typeParams) {

    static final SelfTypeInfo empty = new SelfTypeInfo(Option.empty(), ScalaConversions.nil());

    static SelfTypeInfo fromTypeDefinition(Definition.Type d) {
      // TODO currently the `Self` type is only used internally as an ascription for static method
      // bindings
      //  Once we actually start supporting the `Self` syntax, we should set the self type here to
      // the ResolvedType
      //  corresponding to the current definition, so that we can correctly resolve `Self`
      // references in constructor
      //  argument types.
      var selfType = Option.<BindingsMap.ResolvedType>empty();
      var typeParams = d.params().map(p -> p.name());
      return new SelfTypeInfo(selfType, typeParams);
    }

    static SelfTypeInfo fromMethodReference(Name.MethodReference m) {
      if (m.typePointer().isDefined()) {
        var p = m.typePointer().get();
        var resolution =
            MetadataInteropHelpers.getMetadataOrNull(
                m, MethodDefinitions.INSTANCE, BindingsMap.Resolution.class);
        // It is unexpected that the metadata is missing here, but we don't fail because other
        // passes should fail
        // with more detailed info.
        if (resolution != null) {
          return switch (resolution.target()) {
            case BindingsMap.ResolvedType typ -> {
              var params =
                  typ.tp()
                      .params()
                      .map(
                          n ->
                              new Name.Literal(
                                  n, false, null, Option.empty(), new MetadataStorage()))
                      .toList();
              yield new SelfTypeInfo(Option.apply(typ), params);
            }
            case BindingsMap.ResolvedModule __ -> SelfTypeInfo.empty;
            case Object other -> throw new CompilerError(
                "Method target not resolved as ResolvedType, but %s.".formatted(other));
          };
        }
      }
      return empty;
    }
  }

  private static <T extends IR> T doResolveType(
      SelfTypeInfo selfTypeInfo, BindingsMap bindingsMap, T ir) {
    var s =
        MetadataInteropHelpers.getMetadataOrNull(
            ir, TypeSignatures$.MODULE$, TypeSignatures.Signature.class);
    if (s != null) {
      var meta =
          new TypeSignatures.Signature(
              resolveSignature(selfTypeInfo, bindingsMap, s.signature()), s.comment());
      MetadataInteropHelpers.updateMetadata(ir, TypeSignatures$.MODULE$, meta);
    }
    return ir;
  }

  private static Expression resolveSignature(
      SelfTypeInfo selfTypeInfo, BindingsMap bindingsMap, Expression expression) {
    return expression.mapExpressions(
        (expr) -> {
          if (SuspendedArguments.representsSuspended(expr)) {
            return expr;
          } else {
            return switch (expr) {
              case Name.Literal n -> {
                if (selfTypeInfo
                    .typeParams()
                    .exists(
                        p -> {
                          return p.name().equals(n.name());
                        })) {
                  yield n;
                } else {
                  var rn = bindingsMap.resolveName(n.name());
                  yield processResolvedName(n, rn);
                }
              }
              case Name.Qualified n -> {
                var parts = bindingsMap.resolveQualifiedName(n.parts().map(p -> p.name()));
                yield processResolvedName(n, parts);
              }
              case Name.SelfType selfRef -> {
                Either<
                        BindingsMap.ResolutionError,
                        scala.collection.immutable.List<? extends BindingsMap.ResolvedName>>
                    resolvedSelfType;
                if (selfTypeInfo.selfType().isEmpty()) {
                  resolvedSelfType =
                      new Left<>(BindingsMap.SelfTypeOutsideOfTypeDefinition$.MODULE$);
                } else {
                  var list = ScalaConversions.set(selfTypeInfo.selfType().get()).toList();
                  resolvedSelfType = new Right<>(list);
                }
                yield processResolvedName(selfRef, resolvedSelfType);
              }
              case Set s -> {
                yield s.mapExpressions(
                    (sig) -> {
                      return resolveSignature(selfTypeInfo, bindingsMap, sig);
                    });
              }
              default -> expr;
            };
          }
        });
  }

  private static Name processResolvedName(
      Name name,
      Either<
              BindingsMap.ResolutionError,
              ? extends scala.collection.immutable.List<? extends BindingsMap.ResolvedName>>
          resolvedNamesOpt) {
    var either =
        resolvedNamesOpt.map(
            (resolvedNames) -> {
              resolvedNames.foreach(
                  resolvedName -> {
                    MetadataInteropHelpers.updateMetadata(
                        name, INSTANCE, new Resolution(resolvedName));
                    return null;
                  });
              return name;
            });
    return either.fold(
        (error) -> {
          var res = new org.enso.compiler.core.ir.expression.errors.Resolution.ResolverError(error);
          return new org.enso.compiler.core.ir.expression.errors.Resolution(
              name, res, new MetadataStorage());
        },
        (n) -> {
          var meta = MetadataInteropHelpers.getMetadata(n, INSTANCE, Resolution.class);
          return switch (meta.target()) {
            case ResolvedModule rm -> {
              var res =
                  new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedModule(
                      "type signature");
              yield new org.enso.compiler.core.ir.expression.errors.Resolution(
                  name, res, new MetadataStorage());
            }
            default -> n;
          };
        });
  }
}
