package org.enso.compiler.pass.resolve;

import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.expression.errors.Conversion;
import org.enso.compiler.core.ir.expression.errors.Conversion.UnsupportedSourceType$;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.Resolution;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedConversionMethod;
import org.enso.compiler.data.BindingsMap.ResolvedExtensionMethod;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.data.BindingsMap.ResolvedModuleMethod;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotField;
import org.enso.compiler.data.BindingsMap.ResolvedPolyglotSymbol;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.pass.desugar.ComplexType$;
import org.enso.compiler.pass.desugar.FunctionBinding$;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Resolves the correct {@code self} argument type for method definitions and stores the resolution
 * in the method's metadata.
 *
 * <p>Metadata type is {@link BindingsMap.Resolution}
 */
public final class MethodDefinitions implements MiniPassFactory {
  private MethodDefinitions() {}

  public static final MethodDefinitions INSTANCE = new MethodDefinitions();

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    var bindingsMap = moduleContext.bindingsAnalysis();
    assert bindingsMap != null;
    return new Mini(bindingsMap);
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    java.util.List<IRProcessingPass> passes =
        java.util.List.of(
            ComplexType$.MODULE$,
            FunctionBinding$.MODULE$,
            GenerateMethodBodies$.MODULE$,
            BindingAnalysis$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRProcessingPass> invalidatedPasses() {
    Object obj = scala.collection.immutable.Nil$.MODULE$;
    return (scala.collection.immutable.List<IRProcessingPass>) obj;
  }

  private static <T> List<T> list(T item) {
    return CollectionConverters.asScala(java.util.List.of(item)).toList();
  }

  private static final class Mini extends MiniIRPass {
    private final BindingsMap bindingsMap;

    private Mini(BindingsMap bindingsMap) {
      this.bindingsMap = bindingsMap;
    }

    @Override
    public Expression transformExpression(Expression expr) {
      throw new IllegalStateException("unreachable - prepare returns null.");
    }

    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      // Supports only transformModule
      return null;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      var newDefs =
          moduleIr
              .bindings()
              .map(
                  def -> {
                    if (def instanceof Method method) {
                      var methodRef = method.methodReference();
                      Option<Name> resolvedTypeRef =
                          methodRef.typePointer().map(tp -> resolveType(tp, bindingsMap));
                      var resolvedMethodRef = methodRef.copyWithTypePointer(resolvedTypeRef);

                      return switch (method) {
                        case Method.Explicit explicitMethod -> {
                          var resolvedMethod =
                              explicitMethod.copy(
                                  resolvedMethodRef,
                                  explicitMethod.body(),
                                  explicitMethod.isStatic(),
                                  explicitMethod.isPrivate(),
                                  explicitMethod.isStaticWrapperForInstanceMethod(),
                                  explicitMethod.location(),
                                  explicitMethod.passData(),
                                  explicitMethod.diagnostics(),
                                  explicitMethod.id());
                          yield resolvedMethod;
                        }
                        case Method.Conversion conversionMethod -> {
                          var sourceTypeExpr = conversionMethod.sourceTypeName();
                          Name resolvedName =
                              switch (sourceTypeExpr) {
                                case Name name -> resolveType(name, bindingsMap);
                                default -> new Conversion(
                                    sourceTypeExpr,
                                    UnsupportedSourceType$.MODULE$,
                                    new MetadataStorage());
                              };
                          var resolvedMethod =
                              conversionMethod.copy(
                                  resolvedMethodRef,
                                  resolvedName,
                                  conversionMethod.body(),
                                  conversionMethod.location(),
                                  conversionMethod.passData(),
                                  conversionMethod.diagnostics(),
                                  conversionMethod.id());
                          yield resolvedMethod;
                        }
                        default -> throw new CompilerError(
                            "Unexpected method type in MethodDefinitions pass.");
                      };
                    } else {
                      return def;
                    }
                  });

      return moduleIr.copyWithBindings(newDefs);
    }

    private Name resolveType(Name typePointer, BindingsMap availableSymbolsMap) {
      if (typePointer instanceof Name.Qualified || typePointer instanceof Name.Literal) {
        var items =
            switch (typePointer) {
              case Name.Qualified qualName -> qualName.parts().map(Name::name);
              case Name.Literal lit -> list(lit.name());
              default -> throw new CompilerError("Impossible to reach");
            };

        var resolvedItemsOpt = availableSymbolsMap.resolveQualifiedName(items);
        if (resolvedItemsOpt.isLeft()) {
          var err = resolvedItemsOpt.swap().toOption().get();
          return new org.enso.compiler.core.ir.expression.errors.Resolution(
              typePointer,
              new org.enso.compiler.core.ir.expression.errors.Resolution.ResolverError(err),
              new MetadataStorage());
        }
        var resolvedItems = resolvedItemsOpt.toOption().get();
        assert resolvedItems.size() == 1 : "Expected a single resolution";
        switch (resolvedItems.head()) {
          case ResolvedConstructor ignored -> {
            return new org.enso.compiler.core.ir.expression.errors.Resolution(
                typePointer,
                new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedConstructor(
                    "a method definition target"),
                new MetadataStorage());
          }
          case ResolvedModule resMod -> {
            MetadataInteropHelpers.updateMetadata(typePointer, INSTANCE, new Resolution(resMod));
            return typePointer;
          }
          case ResolvedType resType -> {
            MetadataInteropHelpers.updateMetadata(typePointer, INSTANCE, new Resolution(resType));
            return typePointer;
          }
          case ResolvedPolyglotSymbol ignored -> {
            return new org.enso.compiler.core.ir.expression.errors.Resolution(
                typePointer,
                new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedPolyglot(
                    "a method definition target"),
                new MetadataStorage());
          }
          case ResolvedPolyglotField ignored -> {
            return new org.enso.compiler.core.ir.expression.errors.Resolution(
                typePointer,
                new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedPolyglot(
                    "a method definition target"),
                new MetadataStorage());
          }
          case ResolvedModuleMethod ignored -> {
            return new org.enso.compiler.core.ir.expression.errors.Resolution(
                typePointer,
                new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedMethod(
                    "a method definition target"),
                new MetadataStorage());
          }
          case ResolvedExtensionMethod ignored -> {
            return new org.enso.compiler.core.ir.expression.errors.Resolution(
                typePointer,
                new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedMethod(
                    "a static method definition target"),
                new MetadataStorage());
          }
          case ResolvedConversionMethod ignored -> {
            return new org.enso.compiler.core.ir.expression.errors.Resolution(
                typePointer,
                new org.enso.compiler.core.ir.expression.errors.Resolution.UnexpectedMethod(
                    "a conversion method definition target"),
                new MetadataStorage());
          }
          default -> throw new IllegalStateException("Unexpected value: " + resolvedItems.head());
        }
      } else if (typePointer instanceof org.enso.compiler.core.ir.expression.errors.Resolution) {
        return typePointer;
      } else {
        throw new CompilerError("Unexpected kind of name for method reference");
      }
    }
  }
}
